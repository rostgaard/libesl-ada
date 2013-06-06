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
with Ada.Task_Attributes;
with Ada.Task_Identification;
with Ada.Task_Termination;
with Ada.Exceptions;
with Ada.IO_Exceptions;
--  with Ada.Strings.Unbounded;
--  with Ada.Containers.Vectors;

with ESL.Trace;
with ESL.Packet;
with ESL.Parsing_Utilities;

package body ESL.Client.Tasking is
   use ESL;
   use Ada.Strings.Unbounded;

   type Client_Event_Listeners is array (ESL.Packet_Keys.Inbound_Events)
     of aliased Event_Streams;

   type Client_Sub_Event_Listeners is array
     (ESL.Packet_Keys.Inbound_Sub_Events) of aliased Event_Streams;

   type Client_Data is
      record
         Client_Ref          : Client.Reference;
         Event_Observers     : access Client_Event_Listeners;
         Sub_Event_Observers : access Client_Sub_Event_Listeners;
         Channel_List        : ESL.Channel.List.Reference;
      end record;

   package Client_Attribute is new Ada.Task_Attributes
     (Attribute => Client_Data, Initial_Value =>
        (Client_Ref          => null,
         Event_Observers     => null,
         Sub_Event_Observers => null,
         Channel_List        => null));

--     procedure Notify (Event  : in AMI.Event.Event_Type;
--                          Packet : in AMI.Parser.Packet_Type);

   protected Shutdown_Handler is
      procedure Termination_Finalizer
        (Cause : in Ada.Task_Termination.Cause_Of_Termination;
         T     : in Ada.Task_Identification.Task_Id;
         X     : in Ada.Exceptions.Exception_Occurrence);
   end Shutdown_Handler;

--     procedure Dispatch (Ref    : in Client.Reference;
--                         Packet : in AMI.Parser.Packet_Type);

   procedure Authenticate (Obj     : in out Instance;
                           Password : in     String) is
      Attr : Client_Data renames Client_Attribute.Value (T => Obj'Identity);
   begin
      Attr.Client_Ref.Authenticate (Password);
   end Authenticate;

   procedure Connect (Obj      : in Instance;
                      Hostname : in String;
                      Port     : in Natural) is
      Attr : Client_Data renames Client_Attribute.Value (T => Obj'Identity);
   begin
      Attr.Client_Ref.Connect (Hostname, Port);
   end Connect;

   --------------
   --  Create  --
   --------------

   function Create return Instance is

   begin
      return Obj : Instance do
         Client_Attribute.Set_Value
           (Val =>
              (Client_Ref          => Client.Create,
               Event_Observers     => new Client_Event_Listeners,
               Sub_Event_Observers => new Client_Sub_Event_Listeners,
               Channel_List        => new Channel.List.Instance),
            T   => Obj'Identity);
         Ada.Task_Termination.Set_Specific_Handler
           (T       => Obj'Identity,
            Handler => Shutdown_Handler.Termination_Finalizer'Access);
      end return;
   end Create;

   procedure Disconnect (Obj : in Instance) is
      Attr : Client_Data renames Client_Attribute.Value (T => Obj'Identity);

   begin
      Attr.Client_Ref.Disconnect;
   end Disconnect;

   function Event_Stream (Client : in Instance;
                          Stream : in ESL.Packet_Keys.Inbound_Events)
                          return Event_Streams_Access is
      Attr : Client_Data renames
        Client_Attribute.Value (T => Client'Identity);
   begin
      return Attr.Event_Observers (Stream)'Access;
   end Event_Stream;

   function Channel_List (Obj : in Instance) return Channel.List.Reference is
      Attr : Client_Data renames Client_Attribute.Value (T => Obj'Identity);
   begin
      return Attr.Channel_List;
   end Channel_List;

   function Sub_Event_Stream (Client : in Instance;
                              Stream : in ESL.Packet_Keys.Inbound_Sub_Events)
                              return Event_Streams_Access is
      Attr : Client_Data renames
        Client_Attribute.Value (T => Client'Identity);
   begin
      return Attr.Sub_Event_Observers (Stream)'Access;
   end Sub_Event_Stream;

   ----------------
   --  Dispatch  --
   ----------------

--     procedure Dispatch (Ref    : in Client.Reference;
--                         Packet : in AMI.Parser.Packet_Type) is
--        Context : constant String := Package_Name & ".Dispatch";
--        pragma Unreferenced (Context);
--        use AMI.Packet_Keys;
--
--        Attr : Client_Data renames Client_Attribute.Value;
--
--     begin
--
--        if Packet.Header.Key = AMI.Packet_Keys.Event then
--           --  Notify the local observers.
--           Notify (Event     => AMI.Event.Event_Type'Value
--                   (To_String (Packet.Header.Value)),
--                   Packet    => Packet);
--           --  Notify the global observers.
--           AMI.Observers.Notify (AMI.Event.Event_Type'Value
--                                 (To_String (Packet.Header.Value)),
--                                 Packet);
--        end if;
--     end Dispatch;

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
         Ref     : Client.Reference := Client_Attribute.Value.Client_Ref;

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

         Client.Deallocate (Ref);

      end Termination_Finalizer;

   end Shutdown_Handler;

   ---------------------
   --  Instance Task  --
   ---------------------

   task body Instance is
      use Ada.Calendar;
      use ESL.Observer;

      function Current_Time return Time renames Clock;
      procedure Reader_Loop;

      Next_Attempt    : Time := Current_Time;
--      Event_Observers : Client_Event_Listeners;

      use Ada.Task_Identification;

      Client : Reference renames Client_Attribute.Value.Client_Ref;
      Attr   : Client_Data renames Client_Attribute.Value;

      Context : constant String :=
        Package_Name & ".Instance(" & Image (Current_Task) & ")";

      procedure Reader_Loop is
      begin
         loop
            Trace.Debug (Context => Context,
                         Message => "Waiting for connection...");
            Client.Wait_For_Connection (Timeout => 3.0);
            Trace.Debug (Context => Context,
                         Message => "Connection ok!");

            declare
               Packet : constant ESL.Packet.Instance :=
                 ESL.Parsing_Utilities.Read_Packet (Stream => Client.Stream);
            begin
               if Packet.Is_Event then
                  Trace.Debug (Context => Context,
                               Message => Packet.Content_Type'Img);
                  Notify_Observers
                    (Observing => Attr.Event_Observers (Packet.Event),
                     Packet    => Packet,
                     Client    => Client);
               end if;

            end;
         end loop;
--           Dispatch (Ref    => Client,
--                     Packet => Client.Read_Packet);
      exception
         when Ada.IO_Exceptions.End_Error =>
            Trace.Debug (Context => Context,
                             Message => "Reader operated on closed socket");
            Client.Connected := False;
         when Connection_Timeout =>
            Trace.Debug (Context => Context,
                             Message => "Timeout reached for reader");
      end Reader_Loop;

   begin
      while not Client.Shutdown loop
         delay until Next_Attempt;
         Next_Attempt := Next_Attempt + Recheck_Connection_Delay;
         Reader_Loop;
      end loop;
   end Instance;

   --------------
   --  Notify  --
   --------------

--     procedure Notify (Event  : in AMI.Event.Event_Type;
--                       Packet : in AMI.Parser.Packet_Type) is
--        Context : constant String := Package_Name & ".Notify ";
--        use Client_Callback_Collections;
--
--        procedure Call (C : Cursor);
--
--        Attr : Client_Data renames
--          Client_Attribute.Value;
--
--        procedure Call (C : Cursor) is
--        begin
--           Element (C) (Attr.Client_Ref, Packet);
--        end Call;
--
--     begin
--        if Attr.Event_Observers (Event).Is_Empty then
--         AMI.Trace.Debug ("Nobody cared about event " & Event'Img, Context);
--        end if;
--
--        Attr.Event_Observers (Event).Iterate (Process => Call'Access);
--     end Notify;

   procedure Send (Obj    : in Instance;
                   Packet : in ESL.Command.Instance'Class) is
   begin
      Client_Attribute.Value
        (Obj'Identity).Client_Ref.Send (String (Packet.Serialize));
   end Send;

   procedure Send (Obj    : in Instance;
                   Packet : in String) is
   begin
      Client_Attribute.Value (Obj'Identity).Client_Ref.Send (Packet);
   end Send;

   ---------------
   --  Shutdown --
   ---------------

   procedure Shutdown (Obj : in Instance) is
      Client : Reference renames
        Client_Attribute.Value (T => Obj'Identity).Client_Ref;
   begin
      Client.Shutdown := True;
      Client.Disconnect;
   end Shutdown;

--     -----------------
--     --  Subscribe  --
--     -----------------
--
--     procedure Subscribe (Obj      : in Instance;
--                          Event    : in ESL.Packet_Keys.Inbound_Events;
--                          Observer : in
--  ESL.Observer.Event_Listener_Reference) is
--        Attr : Client_Data renames
--          Client_Attribute.Value (T => Obj'Identity);
--     begin
--           Attr.Event_Observers (Event).Append (New_Item => Observer);
--     end Subscribe;

end ESL.Client.Tasking;
