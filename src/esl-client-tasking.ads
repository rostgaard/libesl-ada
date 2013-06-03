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
with ESL.Command;
with ESL.Packet_Keys;

package ESL.Client.Tasking is
   task type Instance;

   function Create return Instance;

   type Event_Streams is new ESL.Observer.Observables with private;
   type Event_Streams_Access is access all Event_Streams;

   procedure Authenticate (Obj     : in out Instance;
                           Password : in     String);

   procedure Send (Obj    : in Instance;
                   Packet : in String);
   pragma Obsolescent (Send);

   procedure Send (Obj    : in Instance;
                   Packet : in ESL.Command.Instance);
   --  function Send (Packet : AMI.Packet.Action.Request)
   --  return AMI.Packet.Reponse;

   procedure Connect (Obj      : in Instance;
                      Hostname : in String;
                      Port     : in Natural);

   procedure Disconnect (Obj : in Instance);

   procedure Shutdown (Obj : in Instance);

   function Event_Stream (Client : in Instance;
                          Stream : in ESL.Packet_Keys.Inbound_Events)
                          return Event_Streams_Access;

private
   type Event_Streams is new ESL.Observer.Observables with
     null record;

   Recheck_Connection_Delay : constant Duration := 2.0;
   --  How long we should wait between connection polling.
end ESL.Client.Tasking;
