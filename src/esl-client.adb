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

with Ada.Calendar;

with ESL.Parsing_Utilities;
with ESL.Trace;
with Ada.Exceptions;

package body ESL.Client is
   use ESL.Trace;
   use ESL;

   -----------
   --  "="  --
   -----------

   function "=" (Left, Right : in Reference) return Boolean is
   begin
      return Left.Socket = Right.Socket;
   end "=";

   --------------------
   --  Authenticate  --
   --------------------

   procedure Authenticate (Obj     : in out Instance;
                          Password : in     String) is
   begin
      if not Obj.Connected then
         raise Not_Connected;
      elsif Obj.Authenticated then
         return;
      end if;

      Obj.Send ("auth " & Password & ESL.End_Packet_String);

   end Authenticate;

   function Channel_List (Obj : in Instance) return Channel.List.Reference is
   begin
      return Obj.Channels;
   end Channel_List;

   ---------------
   --  Connect  --
   ---------------

   procedure Connect (Client   : in out Instance;
                      Hostname : in     String;
                      Port     : in     Natural) is
      use Ada.Exceptions;

      Context : constant String := Package_Name & ".Connect";
      Address : Sock_Addr_Type (Family_Inet);
      Socket  : Socket_Type;
      Status  : GNAT.Sockets.Selector_Status;
   begin
      Create_Socket (Socket);

      Client.Socket := Socket;

      Client.Connected := False;
      Client.Connecting := True;
      Client.Authenticated := False;

      Address.Addr   := Addresses (Get_Host_By_Name (Hostname));
      Address.Port   := Port_Type (Port);

      Trace.Information ("Connecting to " &
                         Hostname & ":" &
                         Positive'Image (Port),
                       Context);

      Connect_Socket (Socket   => Client.Socket,
                      Server   => Address,
                      Timeout  => Connection_Timeout_Duration,
                      Selector => Client.Selector'Access,
                      Status   => Status);

      if Status = Completed then
         Client.Channel := Stream (Client.Socket);
         Client.Connected := True;

         Trace.Information ("Connected to " &
                              Hostname & ":" &
                              Positive'Image (Port)& ".", Context);
      else
         Trace.Information ("Could not connect to " &
                              Hostname & ":" &
                              Positive'Image (Port)& ".", Context);
      end if;

      --  Pull down the connecting flag.
      Client.Connecting := False;

   exception
      when E : GNAT.Sockets.Socket_Error =>
         --  Assert the state
         Client.Connected := False;
         Client.Authenticated := False;
         Client.On_Disconnect_Handler.all;
         Trace.Error (Context => Context, Message =>
                        "Failed to connect: " & Exception_Message (E));
   end Connect;

   -----------------
   --  Connected  --
   -----------------

   function Connected (Client : in Instance) return Boolean is
   begin
      return Client.Connected;
   end Connected;

   --------------
   --  Create  --
   --------------

   function Create return Reference is
   begin
      return new Instance;
   end Create;

   ------------------
   --  Disconnect  --
   ------------------

   procedure Disconnect (Client : in out Instance) is
      Context : constant String := Package_Name & ".Disconnect";
   begin
      if Client.Connecting then
         Abort_Selector (Client.Selector);
      elsif Client.Connected then
         Shutdown_Socket (Client.Socket);
      end if;
   exception
      when Program_Error =>
         ESL.Trace.Error (Message => "Tried to abort a closed socket!",
                          Context => Context);
      when E : others =>
         ESL.Trace.Error (Message => Ada.Exceptions.Exception_Information (E),
                          Context => Context);
   end Disconnect;

   ----------------
   --  Finalize  --
   ----------------

   procedure Finalize (Obj : in out Instance) is
      Context : constant String := Package_Name & ".Finalize";
   begin
      --  Obj.Disconnect;
      Trace.Debug ("Finalize (instance) called for Client ");
   exception
      when E : others =>
         ESL.Trace.Error (Message => Ada.Exceptions.Exception_Information (E),
                          Context => Context);
   end Finalize;

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

   ------------------
   --  Initialize  --
   ------------------

   procedure Initialize (Obj : in out Instance) is
   begin
      Trace.Information ("Initialize (instance) called for new client" &
                         Obj.Initialized'Img);
   end Initialize;
   --------------------
   --  Is_Connected  --
   --------------------

   function Is_Connected (Client  : in Instance) return Boolean is
   begin
      raise Program_Error with "Not supported";
      return False;
   end Is_Connected;

   ---------------
   --  Receive  --
   ---------------

   function Receive (Client : in Instance;
                     Count  : in Natural) return String is
      Buffer : String (1 .. Count);
   begin
      String'Read (Client.Channel, Buffer);

      return Buffer;
   end Receive;

   -------------------
   --  Read_Packet  --
   -------------------

   --  function Read_Packet (Client : access AMI.Client.Instance)
   --                        return AMI.Parser.Packet_Type is
   --     use AMI.Packet_Keys;
   --     use AMI.Parser;
   --     Context        : constant String := Package_Name & ".Read_Packet";
   --     Current_Pair   : Pair_Type       := Null_Pair;
   --     Current_Packet : Packet_Type     := New_Packet;
   --  begin
   --     loop
   --        Current_Pair := AMI.Parser.Parse_Line (Line => Client.Get_Line);

   --        --  We return on an empty line, meaning the packet is complete
   --        if Current_Pair = Empty_Line then
   --           return Current_Packet;

   --           --  Fill the header
   --        elsif Current_Packet.Header = Null_Pair then
   --           Current_Packet.Header (Current_Pair);

   --           --  Or simply insert a key/value pair
   --        elsif Current_Pair.Key /= Null_Key then
   --           Current_Packet.Push (Current_Pair);
   --        else
   --           AMI.Trace.Debug ("Read_Packet: Skipping bad line", Context);
   --        end if;
   --     end loop;
   --  end Read_Packet;

   ------------
   --  Send  --
   ------------

   procedure Send (Client : in Instance;
                   Item   : in String) is
   begin
      Client.Wait_For_Connection;

      ESL.Trace.Information (Message => "Sending: " & Item,
                             Context => "client.Send");

      String'Write (Client.Channel, Item &
                      ASCII.CR & ASCII.LF &
                      ASCII.CR & ASCII.LF);
   end Send;

   ------------
   --  Send  --
   ------------

   procedure Send (Client : in Instance;
                   Item   : in ESL.Command.Instance) is
   begin
      --  AMI.Response.Subscribe (Item);

      Client.Send (String (Item.Serialize));
   end Send;

   ------------
   --  Send  --
   ------------

   --  procedure Send (Client : in Instance;
   --                  Item   : in AMI.AMI_Packet) is
   --  begin
   --     Client.Send (String (Item));
   --  end Send;

   ------------
   --  Send  --
   ------------

   --  function Send (Client : in Instance;
   --                 Item   : in AMI.Packet.Action.Request)
   --                 return AMI.Parser.Packet_Type is
   --  begin
   --     AMI.Response.Subscribe (Item);
   --     Send (Client => Client,
   --           Item   => String (Item.To_AMI_Packet));

   --     return AMI.Response.Claim (Ticket => Item.Action_ID);
   --  end Send;

   procedure Set_Log_Level (Obj   : in out Instance;
                            Level : in     Natural) is
   begin
      --  TODO
      if Level > 0 then
         Obj.Send ("log" & Level'Img & ESL.End_Packet_String);
      else
         Obj.Send ("nolog" & ESL.End_Packet_String);
      end if;
   end Set_Log_Level;

   ----------------
   --  Shutdown  --
   ----------------

   procedure Shutdown (Client : in out Instance) is
   begin
      Client.Shutdown  := True;
      Client.Disconnect;
   end Shutdown;

   -----------------------------
   --  Skip_Until_Empty_Line  --
   -----------------------------

   procedure Skip_Until_Empty_Line (Obj : in Instance) is
   begin
      while Obj.Get_Line'Length > 0 loop
         null;
      end loop;
   end Skip_Until_Empty_Line;

   function Stream (Obj : in Instance)
                       return Ada.Streams.Stream_IO.Stream_Access is
   begin
      return Ada.Streams.Stream_IO.Stream_Access (Obj.Channel);
   end Stream;

   ---------------------------
   --  Wait_For_Connection  --
   ---------------------------

   procedure Wait_For_Connection (Client  : in Instance;
                                  Timeout : in Duration := 3.0) is
      use Ada.Calendar;
      Absolute_Timeout : constant Time := Clock + Timeout;
   begin
      if Client.Connected then
         return;
      end if;

      loop
         exit when
           Client.Connected or
           Clock > Absolute_Timeout or
           Client.Shutdown;
         delay 0.05;

      end loop;

      if not Client.Connected then
         raise Connection_Timeout;
      end if;
   end Wait_For_Connection;
end ESL.Client;
