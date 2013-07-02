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

with ESL.Observer;
with ESL.Packet_Keys;

package ESL.Client.Tasking is

   Package_Name : constant String  := "ESL.Client.Tasking";

   type Instance is new Client.Instance with private;
   type Event_Streams is new ESL.Observer.Observables with null record;

   type Event_Streams_Access is access all Event_Streams;

   function Event_Stream (Client : in Instance;
                          Stream : in ESL.Packet_Keys.Inbound_Events)
                          return Event_Streams_Access;

   function Sub_Event_Stream (Client : in Instance;
                              Stream : in ESL.Packet_Keys.Inbound_Sub_Events)
                              return Event_Streams_Access;

private

   --  TODO: Add initialization and finalization.

   Recheck_Connection_Delay : constant Duration := 2.0;
   --  How long we should wait between connection polling.

   type Reference is access all Instance;

   task type Stream_Reader (Owner : access Client.Instance'Class);

   type Client_Event_Listeners is array (ESL.Packet_Keys.Inbound_Events)
     of aliased Event_Streams;

   type Client_Sub_Event_Listeners is array
     (ESL.Packet_Keys.Inbound_Sub_Events) of aliased Event_Streams;

   type Instance is new Client.Instance with
      record
         Event_Observers     : access Client_Event_Listeners
           := new Client_Event_Listeners;
         Sub_Event_Observers : access Client_Sub_Event_Listeners
           := new Client_Sub_Event_Listeners;
         Reader              : Stream_Reader (Instance'Access);
      end record;

   overriding procedure Initialize (Obj : in out Instance);
   overriding procedure Finalize (Obj : in out Instance);

end ESL.Client.Tasking;
