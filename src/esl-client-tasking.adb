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
with Ada.Task_Identification;
with Ada.Task_Termination;
with Ada.Exceptions;
with Ada.IO_Exceptions;

with ESL.Trace;
with ESL.Packet;
with ESL.Parsing_Utilities;

package body ESL.Client.Tasking is
   use ESL;
   use ESL.Reply;

   task type Dispatchers (Owner : access Client.Tasking.Instance) is
      entry Queue_For_Dispatching (Packet : ESL.Packet.Instance);
   end Dispatchers;
   --  Dispatcher task that frees up the other contexts.

   procedure API (Client  : in out Instance;
                  Command : in     ESL.Command.Instance'Class;
                  Reply   : in out ESL.Reply.Instance) is
   begin
      Client.Synchonous_Operations.Send (Item => "API " & Command.Serialize);
      Client.Synchonous_Operations.Pop_Reply (Item  => Reply);
   end API;

   --------------------
   --  Authenticate  --
   --------------------

   procedure Authenticate (Client   : in out Instance;
                           Password : in     String) is
      Command : constant Serialized_Command :=
        "auth " & Serialized_Command (Password & ESL.End_Packet_String);
      Reply : ESL.Reply.Instance;
   begin
      Client.Synchonous_Operations.Send (Item  => Command);
      ESL.Trace.Debug (Message => "Waiting for reply...",
                       Context => Package_Name & ".Authenticate");

      Client.Synchonous_Operations.Pop_Reply (Item => Reply);

      ESL.Trace.Debug (Message => "Got reply: " & Reply.Response'Img,
                       Context => Package_Name & ".Authenticate");

      if Reply.Response = Error then
         raise Authentication_Failure with
           "Bad credentials.";
      end if;

   end Authenticate;

   procedure Background_API (Client  : in out Instance;
                             Command : in     ESL.Command.Instance'Class;
                             Reply   : in out ESL.Reply.Instance) is
   begin
      Client.Synchonous_Operations.Send ("BGAPI " & Command.Serialize);
      Client.Synchonous_Operations.Pop_Reply (Item  => Reply);
   end Background_API;

   procedure Dispatch (Client : access ESL.Client.Tasking.Instance'Class;
                       Packet : in ESL.Packet.Instance);
   --  Dispatches a collected packet to the appropriate handler(s). Be that
   --  procedures waiting for a reply, or event observers.

   protected Shutdown_Handler is
      procedure Termination_Finalizer
        (Cause : in Ada.Task_Termination.Cause_Of_Termination;
         T     : in Ada.Task_Identification.Task_Id;
         X     : in Ada.Exceptions.Exception_Occurrence);
   end Shutdown_Handler;
   --  Last-breath handler for our internal task. Doesn't serve any other
   --  purpose other than keeping an eye on whether our internal task
   --  behaves inappropriately

   --------------------
   --  Change_State  --
   --------------------

   procedure Change_State (Client    : access Instance;
                           New_State : in     States) is
   begin
      Client.Current_State := New_State;
   end Change_State;

   ---------------
   --  Connect  --
   ---------------

   procedure Connect (Client   : access Instance;
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
      Create_Socket (Socket);
      Client.Socket := Socket;

      Client.Change_State (New_State => Connecting);
      Client.Authenticated := False;

      Trace.Information ("Connecting to " &
                           Hostname & ":" &
                           Positive'Image (Port),
                         Context);

      Connect_Socket (Socket   => Client.Socket,
                      Server   => Address,
                      Timeout  => GNAT.Sockets.Forever,
                      Selector => Client.Selector'Access,
                      Status   => Status);

      if Status = Completed then
         Client.Channel := Stream (Client.Socket);
         Client.Change_State (New_State => Connected);
         Client.Reader := new Stream_Reader (Reference (Client));

         Trace.Information ("Connected to " &
                              Hostname & ":" &
                              Positive'Image (Port)& ".", Context);

         --  Signal the connected event listener.
         Client.On_Connect_Handler.all;
      else
         Trace.Information ("Could not connect to " & Image (Address)
                            & ".", Context);
         Client.Current_State := Disconnected;
      end if;

   exception
      when E : GNAT.Sockets.Socket_Error =>
         --  Pull down the connecting flag.
         --  Assert the state
         Client.Current_State := Disconnected;
         Client.Authenticated := False;
         --  Client.On_Disconnect_Handler.all;
         Trace.Error (Context => Context, Message =>
                      "Failed to connect: " & Exception_Message (E));
   end Connect;

   ------------------
   --  Disconnect  --
   ------------------

   procedure Disconnect (Client : in out Instance) is
      use Ada.Calendar;

      Context : constant String := Package_Name & ".Disconnect";
      Timeout : constant Time   := Clock + 3.0;
   begin
      ESL.Trace.Debug (Message => "Entry state: " & Client.State'Img,
                       Context => Context);

      Client.Change_State (New_State => Disconnecting);

      case Client.State is
         when Connecting =>
            Abort_Selector (Client.Selector);
         when Created =>
            null;
         when others =>
            if Client.Socket /= No_Socket then
               Shutdown_Socket (Client.Socket);
            end if;
      end case;

      while Client.Reader /= null loop
         exit when Clock > Timeout;
         delay 0.01;
      end loop;

      --  At this point, try a hard abort!
      if Client.Reader /= null then
         abort Client.Reader.all;
      end if;

      Client.Change_State (New_State => Disconnected);
      ESL.Trace.Debug (Message => "Exit state: " & Client.State'Img,
                       Context => Context);
   exception
      when Event : Socket_Error =>
         ESL.Trace.Error (Message => "Socket error: " &
                            Ada.Exceptions.Exception_Message (Event) & ".",
                          Context => Context);
      when Program_Error =>
         ESL.Trace.Error (Message => "Tried to abort a closed selector!",
                          Context => Context);
      when E : others =>
         ESL.Trace.Error (Message => Ada.Exceptions.Exception_Information (E),
                          Context => Context);
   end Disconnect;

   -------------------
   --  Dispatchers  --
   -------------------

   task body Dispatchers is
      use Ada.Task_Identification;

      Context : constant String :=
        Package_Name & ".Dispatchers(" & Image (Current_Task) & ")";

      Current_Packet : ESL.Packet.Instance;
   begin
      Ada.Task_Termination.Set_Specific_Handler
        (T => Current_Task,
         Handler => Shutdown_Handler.Termination_Finalizer'Access);

      Trace.Debug
        (Context => Context,
         Message => "Starting.");

      while Owner.State = Connected loop
         select
            accept Queue_For_Dispatching (Packet : ESL.Packet.Instance) do
               Current_Packet := Packet;
               --  Clone the packet to let caller get on with its work.
            end Queue_For_Dispatching;
         or
            terminate;
         end select;

         Owner.Dispatch (Current_Packet);
      end loop;
      Trace.Debug
        (Context => Context,
         Message => "Stopping.");

   end Dispatchers;

   ----------------
   --  Dispatch  --
   ----------------

   procedure Dispatch (Client : access ESL.Client.Tasking.Instance'Class;
                       Packet : in ESL.Packet.Instance) is
      Context : constant String := Package_Name & ".Dispatch";
   begin
      if Packet.Is_Event then
         declare
            Observing : ESL.Observer.Observables renames
              ESL.Observer.Observables
                (Client.Event_Observers (Packet.Event));
         begin
            ESL.Observer.Notify_Observers
              (Observing => Observing,
               Packet    => Packet);
         end;
      elsif Packet.Is_Response then
         Trace.Debug (Context => Context,
                      Message => "Pushing response");

         Client.Synchonous_Operations.Push_Reply
           (Item => ESL.Reply.Create (Packet => Packet));
      end if;

   exception
      when E : others =>
         ESL.Trace.Error (Message => "Dispatch Failed with" &
                            Ada.Exceptions.Exception_Information (E),
                          Context => Context);
         ESL.Trace.Error (Message => "Packet dump follows=====>",
                          Context => Context);
         ESL.Trace.Error (Message => Packet.Raw_Payload,
                          Context => Context);
         ESL.Trace.Error (Message => "<======Packet dump End",
                          Context => Context);
   end Dispatch;

   --------------------
   --  Event_Stream  --
   --------------------

   function Event_Stream (Client : in Reference;
                          Stream : in ESL.Packet_Keys.Inbound_Events)
                          return Event_Streams_Access is
   begin
      return Client.Event_Observers (Stream)'Access;
   end Event_Stream;

   ----------------
   --  Finalize  --
   ----------------

   procedure Finalize (Obj : in out Instance) is
      Context : constant String := Package_Name & ".Finalize";
   begin
      ESL.Trace.Debug (Message => "Entry state: " & Obj.State'Img,
                       Context => Context);

      Obj.Disconnect;
      GNAT.Sockets.Close_Selector (Obj.Selector);
      Obj.Current_State := Finalized;

      ESL.Trace.Debug (Message => "Exit state: " & Obj.State'Img,
                       Context => Context);
   exception
      when E : others =>
         ESL.Trace.Error (Message => Ada.Exceptions.Exception_Information (E),
                          Context => Context);
   end Finalize;

   ----------------
   --  Get_Line  --
   ----------------

   overriding function Get_Line (Client : in Instance) return String is
      pragma Unreferenced (Client);
   begin
      raise Program_Error with "Erronous usage. Only internal object " &
        "may recieve";
      return "";
   end Get_Line;

   ------------------
   --  Initialize  --
   ------------------

   procedure Initialize (Obj : in out Instance) is
   begin
      Create_Selector (Obj.Selector);

      ESL.Trace.Error (Message => "init!",
                       Context => "Initialize");
   end Initialize;

   ---------------
   --  Receive  --
   ---------------

   overriding function Receive (Client : in Instance;
                                Count  : in Natural) return String is
      pragma Unreferenced (Client, Count);
   begin
      raise Program_Error with "Erronous usage. Only internal object " &
        "may recieve";

      return "";
   end Receive;

   ----------------
   --  Shutdown  --
   ----------------

   procedure Shutdown (Client : in out Instance) is
   begin
      Client.Disconnect;
      Client.Current_State := Finalizing;
   end Shutdown;

   -----------------------------
   --  Skip_Until_Empty_Line  --
   -----------------------------

   overriding  procedure Skip_Until_Empty_Line (Client : in Instance) is
      pragma Unreferenced (Client);
   begin
      raise Program_Error with "Erronous usage. Only internal object " &
        "may recieve";
   end Skip_Until_Empty_Line;

   ------------------------
   --  Sub_Event_Stream  --
   ------------------------

   function Sub_Event_Stream (Client : in Instance;
                              Stream : in ESL.Packet_Keys.Inbound_Sub_Events)
                              return Event_Streams_Access is
   begin
      return Client.Sub_Event_Observers (Stream)'Access;
   end Sub_Event_Stream;

   ------------------------
   --  Shutdown_Handler  --
   ------------------------

   protected body Shutdown_Handler is

      procedure Termination_Finalizer
        (Cause : in Ada.Task_Termination.Cause_Of_Termination;
         T     : in Ada.Task_Identification.Task_Id;
         X     : in Ada.Exceptions.Exception_Occurrence)
      is
         use Ada.Task_Termination;
         use Ada.Task_Identification;
         use Ada.Exceptions;

         Context : constant String :=
           Package_Name & ".Shutdown_Handler.Termination_Finalizer";
      begin

         case Cause is
         when Normal =>
            Trace.Debug
              (Context => Context,
               Message => "Task " & Image (T => T) & " terminated normally");
         when Abnormal =>
            Trace.Error
              (Context => Context,
               Message => "Task " & Image (T => T) &
                 " terminated abnormally.");
         when Unhandled_Exception =>
            Trace.Error
              (Context => Context,
               Message => "Task " & Image (T => T) &
                 " terminated with exception: " & Exception_Information (X));
         end case;

      end Termination_Finalizer;

   end Shutdown_Handler;

   --------------------------
   --  Stream_Reader Task  --
   --------------------------

   task body Stream_Reader is
      use Ada.Calendar;
      use Ada.Task_Identification;
      use ESL.Observer;

      Context : constant String :=
        Package_Name & ".Instance(" & Image (Current_Task) & ")";

      type Disp_Index is mod 8;
      Current : Disp_Index := Disp_Index'First;

      function Current_Time return Time renames Clock;
      procedure Reader_Loop;

      Next_Attempt    : Time := Current_Time;
      Dispatcher      : array (Disp_Index) of Dispatchers (Owner);
      --  Pass the reference on.

      procedure Reader_Loop is
      begin
         Owner.Wait_For_Connection (Timeout => 2.0);

         loop
            declare
               Packet : constant ESL.Packet.Instance :=
                 ESL.Parsing_Utilities.Read_Packet (Stream => Owner.Stream);
            begin
               Dispatcher (Current)
                 .Queue_For_Dispatching (Packet => Packet);
               Current := Disp_Index'Succ (Current);
            end;
         end loop;
      exception
            --  These are expected behaviour. If the recieve call fails, we
            --  Expect the client to get back on its feet again or shut down.
         when Ada.IO_Exceptions.End_Error | GNAT.Sockets.Socket_Error =>
            Trace.Information
              (Context => Context,
               Message => "Reader operated on closed socket.");
            Owner.Signal_Disconnect;

         when Connection_Timeout =>
            Trace.Error
              (Context => Context,
               Message =>
               "Timeout reached while wating for a connection channel.");

         when others =>
            Trace.Error
              (Context => Context,
               Message =>
               "Not connected!");
      end Reader_Loop;

   begin
      Ada.Task_Termination.Set_Specific_Handler
        (T => Current_Task,
         Handler => Shutdown_Handler.Termination_Finalizer'Access);

      Trace.Information (Context => Context,
                         Message => "Starting stream consumer.");

      while Owner.Current_State = Connected loop
         delay until Next_Attempt;
         Next_Attempt := Next_Attempt + Recheck_Connection_Delay;
         Trace.Information (Context => Context,
                            Message => "stream consumer." & Owner.State'Img);
         Reader_Loop;
      end loop;

      Trace.Information (Context => Context,
                         Message => "Ending stream consumer.");

      Owner.Reader := null;
   exception
      when Connection_Timeout =>
         --  Regular shutdown.
         null;
      when E : others =>
         Trace.Critical (Context => Context,
                         Message => "Unhandled Error!");
         Trace.Critical (Context => Context,
                         Message => Ada.Exceptions.Exception_Information (E));
   end Stream_Reader;

   -----------------------
   --  Synchronized_IO  --
   -----------------------

   protected body Synchronized_IO is

      procedure Push_Reply (Item : Reply.Instance) is
         Context : constant String := Package_Name &
           ".Synchronized_IO.Push_Reply";
      begin
         ESL.Trace.Debug (Message => "Pushing reply:" & Item.Response'Img,
                          Context => Context);
         Next_Reply := Item;
      end Push_Reply;

      entry Pop_Reply (Item : out Reply.Instance)
        when Next_Reply /= Null_Reply is
      begin
         Item       := Next_Reply;
         Next_Reply := Null_Reply;
      end Pop_Reply;

      entry Discard_Reply when Next_Reply /= Null_Reply is
      begin
         Next_Reply := Null_Reply;
      end Discard_Reply;

      procedure Send (Item  : in Serialized_Command) is
         Context : constant String := Package_Name &
           ".Synchronized_IO.Send";
         pragma Unreferenced (Context);

      begin
         Next_Reply := Null_Reply;
         --  Clear the reply, so we
         --  do not receive the previous reply by mistake.
         Owner.Send (String (Item));
      end Send;

   end Synchronized_IO;

   --------------------
   --  Unmute_Event  --
   --------------------

   procedure Unmute_Event (Client : in out Instance;
                           Event  : in     ESL.Packet_Keys.Inbound_Events) is
      Request : constant Serialized_Command :=
        Serialized_Command ("event plain " & Event'Img);
   begin
      Client.Synchonous_Operations.Send (Item => Request);
      Client.Synchonous_Operations.Discard_Reply;
   end Unmute_Event;

end ESL.Client.Tasking;
