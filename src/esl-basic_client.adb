-------------------------------------------------------------------------------
--                                                                           --
--                     Copyright (C) 2012-, AdaHeads K/S                     --
--                                                                           --
--  This is free software;  you can redistribute it and/or modify it         --
--  under terms of the  GNU General Public License  as published by the      --
--  Free Software  Foundation;  either version 3,  or (at your  option) any  --
--  later version. This library is distributed in the hope that it will be   --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     --
--  You should have received a copy of the GNU General Public License and    --
--  a copy of the GCC Runtime Library Exception along with this program;     --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
--  <http://www.gnu.org/licenses/>.                                          --
--                                                                           --
-------------------------------------------------------------------------------

with Ada.Calendar,
     Ada.Exceptions;

with ESL.Packet,
     ESL.Packet_Content_Type,
     ESL.Parsing_Utilities,
     ESL.Trace;

package body ESL.Basic_Client is

   use ESL,
       ESL.Trace;

   -------------
   --  Mutex  --
   -------------

   protected body Mutex is
      entry Seize when not Owned is
      begin
         Owned := True;
      end Seize;
      procedure Release is
      begin
         Owned := False;
      end Release;
   end Mutex;

   -----------
   --  API  --
   -----------

   procedure API (Client  : in out Instance;
                  Command : in     ESL.Command.Instance'Class;
                  Reply   : in out ESL.Reply.Instance) is
      use ESL.Reply;
      use ESL.Packet_Content_Type;

      Packet    : ESL.Packet.Instance;
   begin
      Reply := ESL.Reply.Null_Reply;
      Client.IO_Mutex.Seize;

      Client.Send (Known_Commands'Image (API) & " " &
                     String (Command.Serialize));

      while Packet.Content_Type /= API_Response loop
         Packet := ESL.Parsing_Utilities.Read_Packet
           (Stream => Client.Stream);
      end loop;

      Reply := ESL.Reply.Create (Packet => Packet);
      Client.IO_Mutex.Release;
   exception
      when others =>
         Client.IO_Mutex.Release;
         raise;
   end API;

   --------------------
   --  Authenticate  --
   --------------------

   procedure Authenticate (Client   : in out Instance;
                           Password : in     String) is
      use ESL.Reply;

      Command : constant Serialized_Command :=
        "auth " & Serialized_Command (Password & ESL.End_Packet_String);

   begin

      Client.IO_Mutex.Seize;
      Client.Send (Command);

      declare
         Got_Reply : Boolean := False;
         Packet    : ESL.Packet.Instance := ESL.Packet.Empty_Packet;
      begin
         while not Got_Reply loop
            Packet := ESL.Parsing_Utilities.Read_Packet
              (Stream => Client.Stream);
            if Packet.Is_Response then
               Got_Reply := True;
               if ESL.Reply.Create (Packet).Response = Error then
                  raise Authentication_Failure with "Bad credentials.";
               end if;
            end if;
         end loop;
      end;

      Client.IO_Mutex.Release;
   exception
      when others =>
         Client.IO_Mutex.Release;
         raise;

   end Authenticate;

   ----------------------
   --  Background_API  --
   ----------------------

   procedure Background_API (Client  : in     Instance;
                             Command : in     ESL.Command.Instance'class) is
   begin
      Client.Send (Known_Commands'Image (BGAPI) & " " &
                     String (Command.Serialize));
   end Background_API;

   --------------------
   --  Change_State  --
   --------------------

   procedure Change_State (Object    :    out Instance;
                           New_State : in     States) is
   begin
      Object.Current_State := New_State;
   end Change_State;

   ---------------
   --  Connect  --
   ---------------

   procedure Connect (Object   : in out Instance;
                      Hostname : in     String;
                      Port     : in     Natural) is
      use Ada.Exceptions;

      Context : constant String := Package_Name & ".Connect";
      Address : constant Sock_Addr_Type :=
        (Family => GNAT.Sockets.Family_Inet,
         Addr   => Addresses (Get_Host_By_Name (Hostname)),
         Port   => Port_Type (Port));

      Socket   : Socket_Type;
      Status   : GNAT.Sockets.Selector_Status;
   begin

      Object.IO_Mutex.Seize;

      GNAT.Sockets.Close_Selector (Object.Selector);
      --  Assert state, since GNAT.Sockets does not provide any intuitive
      --  way of checking if a selector is valid or not.

      GNAT.Sockets.Create_Selector (Object.Selector);

      Create_Socket (Socket);
      Object.Socket := Socket;

      Object.Change_State (New_State => Connecting);
      Object.Authenticated := False;

      Trace.Information ("Connecting to " &
                           Hostname & ":" &
                           Positive'Image (Port),
                         Context);

      Connect_Socket (Socket   => Object.Socket,
                      Server   => Address,
                      Timeout  => GNAT.Sockets.Forever,
                      Selector => Object.Selector'Access,
                      Status   => Status);

      if Status = Completed then
         Object.Channel := Stream (Object.Socket);
         Object.Change_State (New_State => Connected);

         Trace.Information ("Connected to " &
                              Hostname & ":" &
                              Positive'Image (Port)& ".", Context);
      else
         Trace.Information ("Could not connect to " & Image (Address)
                            & ".", Context);
         Object.Socket := No_Socket;
         Object.Change_State (New_State => Disconnected);
      end if;

      Object.IO_Mutex.Release;

   exception
      when E : GNAT.Sockets.Socket_Error =>
         Object.IO_Mutex.Release;
         --  Pull down the connecting flag and assert the state
         Object.Change_State (New_State => Disconnected);
         Object.Authenticated := False;
         Trace.Error (Context => Context, Message =>
                      "Failed to connect: " & Exception_Message (E));
   end Connect;

   ------------------
   --  Disconnect  --
   ------------------

   procedure Disconnect (Object : in out Instance) is
   begin
      case Object.State is
         when Connecting =>
            Abort_Selector (Object.Selector);
         when Created =>
            null;
         when others =>
            if Object.Socket /= No_Socket then
               Shutdown_Socket (Object.Socket);
            end if;
      end case;
   exception
      when GNAT.Sockets.Socket_Error =>
         null; -- Ignore errors at this point, as we do not reuse sockets nor
               -- care about what happens to them af they are requested to
               -- to be shut down.
   end Disconnect;

   ----------------
   --  Get_Line  --
   ----------------

   function Get_Line (Client : in Instance) return String is
   begin
      return Parsing_Utilities.Get_Line (Stream => Client.Channel);
   end Get_Line;

   -------------
   --  Image  --
   -------------

   function Image (Client : in Instance) return String is
   begin
      raise Program_Error with "Not implemented!";
      return "";
   end Image;

   ---------------
   --  Receive  --
   ---------------

   function Receive (Client : in Instance;
                     Count  : in Natural) return String is
      Buffer : String (1 .. Count);
   begin
      String'Read (Client.Channel, Buffer);

      return Buffer;
   exception
      when others =>
         ESL.Trace.Error (Message => "Receive failed!.",
                          Context => "ESL.Client.Receive");
         raise;
   end Receive;

   ------------
   --  Send  --
   ------------

   procedure Send (Client : in Instance;
                   Item   : in String) is
   begin
      String'Write (Client.Channel, Item &
                      ASCII.CR & ASCII.LF &
                      ASCII.CR & ASCII.LF);
      ESL.Trace.Debug (Message => "Sent: " & Item,
                       Context => "ESL.Client.Send");
   exception
      when others =>
         ESL.Trace.Error (Message => "Send failed!",
                          Context => "ESL.Client.Send");
   end Send;

   ------------
   --  Send  --
   ------------

   procedure Send (Client : in Instance;
                   Item   : in ESL.Command.Instance'Class) is
   begin
      Client.Send (String (Item.Serialize));
   end Send;

   ------------
   --  Send  --
   ------------

   procedure Set_Log_Level (Obj   : in out Instance;
                            Level : in     Natural) is
   begin

      if Level > 0 then
         Obj.Send ("log" & Level'Img & ESL.End_Packet_String);
      else
         Obj.Send ("nolog" & ESL.End_Packet_String);
      end if;
   end Set_Log_Level;

   -------------------------
   --  Signal_Disconnect  --
   -------------------------

   procedure Signal_Disconnect (Obj : out Instance) is
      Context : constant String := Package_Name & ".Signal_Disconnect";
   begin
      Obj.Change_State (New_State => Disconnected);
      ESL.Trace.Information (Message => "Signaled!",
                             Context => Context);
   end Signal_Disconnect;

   -----------------------------
   --  Skip_Until_Empty_Line  --
   -----------------------------

   procedure Skip_Until_Empty_Line (Obj : in Instance) is
   begin
      while Obj.Get_Line'Length > 0 loop
         null;
      end loop;
   end Skip_Until_Empty_Line;

   -------------
   --  State  --
   -------------

   function State (Client : in Instance) return States is
   begin
      return Client.Current_State;
   end State;

   --------------
   --  Stream  --
   --------------

   function Stream (Obj : in Instance)
                       return Ada.Streams.Stream_IO.Stream_Access is
   begin
      return Ada.Streams.Stream_IO.Stream_Access (Obj.Channel);
   end Stream;

   --------------------
   --  Unmute_Event  --
   --------------------

   procedure Unmute_Event (Client : in out Instance;
                           Format : in     Event_Formats;
                           Event  : in     ESL.Packet_Keys.Inbound_Events) is
   begin
      Client.Unmute_Event (Format => Format,
                           Event  => Event'Img);
   end Unmute_Event;

   --------------------
   --  Unmute_Event  --
   --------------------

   procedure Unmute_Event (Client : in out Instance;
                           Format : in     Event_Formats;
                           Event  : in     String) is
      use ESL.Reply;
      use ESL.Packet_Content_Type;

      Packet : ESL.Packet.Instance;
      Reply  : ESL.Reply.Instance;
   begin
      Reply := ESL.Reply.Null_Reply;
      Client.IO_Mutex.Seize;

      ESL.Trace.Debug ("Unmuting " & Event);

      Client.Send ("event" &
                     " " &
                     Format'Img &
                     " " &
                     Event);

      --  Wait for the reply.
      while Packet.Content_Type /= Command_Reply loop
         Packet := ESL.Parsing_Utilities.Read_Packet
           (Stream => Client.Stream);
      end loop;

      Reply := ESL.Reply.Create (Packet => Packet);

      if Reply.Response = Error then
         raise Command_Failed with
           Reply.Response_Body;
      end if;

      Client.IO_Mutex.Release;
   exception
      when others =>
         Client.IO_Mutex.Release;
         raise;
   end Unmute_Event;

   ---------------------------
   --  Wait_For_Connection  --
   ---------------------------

   procedure Wait_For_Connection (Client  : in Instance;
                                  Timeout : in Duration := 3.0) is
      use Ada.Calendar;
      Absolute_Timeout : constant Time := Clock + Timeout;
   begin
      case Client.Current_State is
         when Connected =>
            return;
         when Disconnecting .. Finalized =>
            raise Not_Connected;
         when Created .. Connecting =>
            null;
      end case;

      while Client.Current_State = Connecting or
        Clock > Absolute_Timeout
      loop
         delay 0.05;
      end loop;

      if Client.Current_State /= Connected then
         raise Connection_Timeout;
      end if;
   end Wait_For_Connection;

end ESL.Basic_Client;
