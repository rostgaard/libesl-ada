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

   procedure API (Client  : in out Instance;
                  Command : in     ESL.Command.Instance'Class)
   is
   begin
      raise Program_Error with "not implemented!";
   end API;

   --------------------
   --  Authenticate  --
   --------------------

   procedure Authenticate (Client   : in out Instance;
                           Password : in     String;
                           Reply    : in out ESL.Reply.Instance) is
      Command : constant Serialized_Command :=
        "auth " & Serialized_Command (Password & ESL.End_Packet_String);
   begin
      Client.Synchonous_Operations.Send (Item  => Command);
      ESL.Trace.Debug (Message => "Waiting for reply...",
                       Context => Package_Name & ".Authenticate.");

      Client.Synchonous_Operations.Pop_Reply (Item  => Reply);
   end Authenticate;

   procedure Background_API (Client  : in out Instance;
                             Command : in     ESL.Command.Instance'Class;
                             Reply   : in out ESL.Reply.Instance) is
   begin
      Client.Synchonous_Operations.Send (Item  => Command.Serialize);
      Client.Synchonous_Operations.Pop_Reply (Item  => Reply);
      Client.Job_Reply_Buffer.Discard (Reply.UUID);
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
               Packet    => Packet,
               Client    => ESL.Client.Reference (Client));
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
         ESL.Trace.Error (Message => Packet.Payload,
                          Context => Context);
         ESL.Trace.Error (Message => "<======Packet dump End",
                          Context => Context);
   end Dispatch;

   --------------------
   --  Event_Stream  --
   --------------------

   function Event_Stream (Client : in Instance;
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
      pragma Unreferenced (Context);
   begin
      Obj.Shutdown := True;
      ESL.Client.Instance (Obj).Finalize; --  Call the parent finalization.
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
      Client.Shutdown  := True;
      Client.Disconnect;
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

      function Current_Time return Time renames Clock;
      procedure Reader_Loop;

      Next_Attempt    : Time := Current_Time;

      procedure Reader_Loop is
      begin

         Ada.Task_Termination.Set_Specific_Handler
           (T => Current_Task,
            Handler => Shutdown_Handler.Termination_Finalizer'Access);

         loop
            Owner.Wait_For_Connection (Timeout => 3.0);

            declare
               Packet : constant ESL.Packet.Instance :=
                 ESL.Parsing_Utilities.Read_Packet (Stream => Owner.Stream);
            begin
               ESL.Trace.Debug (Message => "Got packet" & Packet.Image,
                                Context => Context);
               Dispatch (Client => Owner,
                         Packet => Packet);
            end;
         end loop;
      exception
         when Ada.IO_Exceptions.End_Error | GNAT.Sockets.Socket_Error =>
            Trace.Error (Context => Context,
                         Message => "Reader operated on closed socket.");
         Owner.Connected := False;
         Owner.Authenticated := False;
         Owner.On_Disconnect_Handler.all;

      when Connection_Timeout =>
            Trace.Debug
              (Context => Context,
               Message =>
               "Timeout reached while wating for a connection channel.");
      end Reader_Loop;

   begin
      Trace.Debug (Context => Context,
                         Message => "Starting stream consumer.");

      while not Owner.Shutdown loop
         delay until Next_Attempt;
         Next_Attempt := Next_Attempt + Recheck_Connection_Delay;
         Reader_Loop;
      end loop;

      Trace.Debug (Context => Context,
                   Message => "Ending stream consumer.");
   exception
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
         ESL.Trace.Debug (Message => "Pushing reply:" & Item.Image,
                          Context => Context);
         Next_Reply := Item;
      end Push_Reply;

      entry Pop_Reply (Item : out Reply.Instance)
        when Next_Reply /= Null_Reply is
      begin
         Item       := Next_Reply;
         Next_Reply := Null_Reply;
      end Pop_Reply;

      procedure Send (Item  : in Serialized_Command) is
         Context : constant String := Package_Name &
           ".Synchronized_IO.Send";
      begin
         Owner.Send (String (Item));
      end Send;

   end Synchronized_IO;

end ESL.Client.Tasking;
