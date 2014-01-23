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

with Ada.Strings.Unbounded;
--  with ESL.Channel.List;
with ESL.Observer.Event_Observers;
with ESL.Packet;

package ESL.Client.Tasking.Test_Utilities is
   use Ada.Strings.Unbounded;

   Package_Name : constant String := "ESL.Client.Tasking.Test_Utilities";

   procedure Signal_Disconnect;
   procedure Signal_Connect;

   Test_Client : aliased ESL.Client.Tasking.Instance
     (On_Connect_Handler    => Signal_Connect'Access,
      On_Disconnect_Handler => Signal_Disconnect'Access);

   procedure Set_Connection_Parameters (New_Hostname : in String;
                                        New_Port     : in Natural;
                                        New_Password : in String);

   type Heartbeat_Observer is
     new ESL.Observer.Event_Observers.Instance with null record;

   overriding
   procedure Notify (Observer : access Heartbeat_Observer;
                     Packet   : in     ESL.Packet.Instance;
                     Client   : in     ESL.Client.Reference);

   type Re_Schedule_Observer is
     new ESL.Observer.Event_Observers.Instance with null record;

   overriding
   procedure Notify (Observer : access Re_Schedule_Observer;
                     Packet   : in     ESL.Packet.Instance;
                     Client   : in     ESL.Client.Reference);
private
   Hostname  : Unbounded_String;
   Password  : Unbounded_String;
   Port      : Natural;
end ESL.Client.Tasking.Test_Utilities;
