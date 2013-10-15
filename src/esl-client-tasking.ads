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

--  with Ada.Containers.Ordered_Sets;

with ESL.Observer;
with ESL.Packet_Keys;
with ESL.Reply;

package ESL.Client.Tasking is

   Package_Name : constant String  := "ESL.Client.Tasking";

   Authentication_Failure : exception;

   type Instance is new Client.Instance with private;
   type Event_Streams is new ESL.Observer.Observables with null record;

   type Event_Streams_Access is access all Event_Streams;

   function Event_Stream (Client : in Instance;
                          Stream : in ESL.Packet_Keys.Inbound_Events)
                          return Event_Streams_Access;

   procedure Unmute_Event (Client : in out Instance;
                           Event  : in     ESL.Packet_Keys.Inbound_Events);

   function Sub_Event_Stream (Client : in Instance;
                              Stream : in ESL.Packet_Keys.Inbound_Sub_Events)
                              return Event_Streams_Access;

   procedure Shutdown (Client : in out Instance);

   procedure Authenticate (Client   : in out Instance;
                           Password : in     String);

   procedure API (Client  : in out Instance;
                  Command : in     ESL.Command.Instance'Class);

   procedure Background_API (Client  : in out Instance;
                             Command : in     ESL.Command.Instance'Class;
                             Reply   : in out ESL.Reply.Instance);

   overriding function Receive (Client : in Instance;
                                Count  : in Natural) return String;
   pragma Obsolescent (Receive, "Illegal usage of Receive, " &
                         "Program_Error will be raise at run-time.");
   overriding function Get_Line (Client : in Instance) return String;
   pragma Obsolescent (Get_Line, "Illegal usage of Get_Line, " &
                         "Program_Error will be raise at run-time.");
   overriding  procedure Skip_Until_Empty_Line (Client : in Instance);
   pragma Obsolescent (Skip_Until_Empty_Line, "Illegal usage of " &
                         "Skip_Until_Empty_Line, " &
                         "Program_Error will be raise at run-time.");
   --  Disables Receiving calls as we are now handling it internally.

private
--   use ESL.Job;

--     package Packet_Storage is new Ada.Containers.Ordered_Sets
--       (Element_Type => ESL.Job.Instance);
--     --  TODO: Make synchronized.

   Recheck_Connection_Delay : constant Duration := 2.0;
   --  How long we should wait between connection polling.

   type Reference is access all Instance;

   task type Stream_Reader (Owner : access Client.Tasking.Instance'Class);

   type Client_Event_Listeners is array (ESL.Packet_Keys.Inbound_Events)
     of aliased Event_Streams;

   type Client_Sub_Event_Listeners is array
     (ESL.Packet_Keys.Inbound_Sub_Events) of aliased Event_Streams;

   protected type Synchronized_IO
     (Owner : access Client.Tasking.Instance'Class) is
      procedure Send (Item  : in Serialized_Command);

      procedure Push_Reply (Item : Reply.Instance);

      entry Discard_Reply;

      entry Pop_Reply (Item : out Reply.Instance);
   private
      Next_Reply : ESL.Reply.Instance := ESL.Reply.Null_Reply;
   end Synchronized_IO;

   type Instance is new Client.Instance with
      record
         Event_Observers         : access Client_Event_Listeners
           := new Client_Event_Listeners;
         Sub_Event_Observers     : access Client_Sub_Event_Listeners
           := new Client_Sub_Event_Listeners;
         Reader                  : Stream_Reader (Instance'Access);
         Sending                 : Boolean := False;
         Synchonous_Operations   : Synchronized_IO (Instance'Access);
      end record;

   overriding procedure Finalize (Obj : in out Instance);

end ESL.Client.Tasking;
