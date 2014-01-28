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

with ESL.Observer.Event_Observers;
with ESL.Packet;
with ESL.Client;

package ESL.Channel.List.Observers is

   Package_Name : constant String := "ESL.Channel.List.Observers";

--  CHANNEL_ANSWER
--  CHANNEL_BRIDGE
--  CHANNEL_CALLSTATE
--  CHANNEL_CREATE
--  CHANNEL_EXECUTE
--  CHANNEL_EXECUTE_COMPLETE
--  CHANNEL_HANGUP
--  CHANNEL_HANGUP_COMPLETE
--  CHANNEL_ORIGINATE
--  CHANNEL_OUTGOING
--  CHANNEL_PROGRESS
--  CHANNEL_PROGRESS_MEDIA
--  CHANNEL_STATE
--  CHANNEL_UNBRIDGE

   type State_Observer is
     new ESL.Observer.Event_Observers.Instance with
      record
         Channel_List : ESL.Channel.List.Reference;
      end record;

   overriding
   procedure Notify (Observer : access State_Observer;
                     Packet   : in     ESL.Packet.Instance;
                     Client   : in     ESL.Client.Reference);

   type Answer_Observer is
     new ESL.Observer.Event_Observers.Instance with
      record
         Channel_List : ESL.Channel.List.Reference;
      end record;

   overriding
   procedure Notify (Observer : access Answer_Observer;
                     Packet   : in     ESL.Packet.Instance;
                     Client   : in     ESL.Client.Reference);

   type Create_Observer is
     new ESL.Observer.Event_Observers.Instance with
      record
         Channel_List : ESL.Channel.List.Reference;
      end record;

   overriding
   procedure Notify (Observer : access Create_Observer;
                     Packet   : in     ESL.Packet.Instance;
                     Client   : in     ESL.Client.Reference);
end ESL.Channel.List.Observers;
