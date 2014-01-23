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

with Ada.Command_Line;
with Ada.Text_IO;

with ESL.Client.Tasking;
with ESL.Packet_Keys;
with ESL.Client.Tasking.Test_Utilities;
with ESL.Channel.List;
with ESL.Channel.List.Observers;
with ESL;
with ESL.Trace;

procedure ESL.Client.Tasking.Channel_List_Test is
   use Ada.Command_Line;
   use Ada.Text_IO;
   use ESL;
   use Client.Tasking.Test_Utilities;

   Testobs1 : Re_Schedule_Observer
     (Observing => Event_Stream (Client => Test_Client'Access,
                                 Stream => ESL.Packet_Keys.RE_SCHEDULE));
   pragma Unreferenced (Testobs1);

   Testobs2 : Heartbeat_Observer
     (Observing => Event_Stream (Client => Test_Client'Access,
                                 Stream => ESL.Packet_Keys.HEARTBEAT));
   pragma Unreferenced (Testobs2);

   CO : ESL.Channel.List.Observers.Create_Observer
     (Observing => Event_Stream (Client => Test_Client'Access,
                                 Stream => ESL.Packet_Keys.CHANNEL_CREATE));
   pragma Unreferenced (CO);

   SO : ESL.Channel.List.Observers.State_Observer
     (Observing => Event_Stream (Client => Test_Client'Access,
                                 Stream => ESL.Packet_Keys.CHANNEL_STATE));
   pragma Unreferenced (SO);

   procedure Usage;

   procedure Usage is
   begin
      Put_Line ("Usage:" & Command_Name & " hostname port password");
      Set_Exit_Status (Failure);
   end Usage;

begin

   ESL.Trace.Unmute (Trace => ESL.Trace.Every);

   if Argument_Count < 3 then
      Usage;
      ESL.Client.Tasking.Shutdown (Client => Test_Client);
      return;
   end if;

   Connect (Test_Client, Argument (1), Natural'Value (Argument (2)));
   Authenticate (Test_Client, Password => Argument (3));

   Send (Test_Client, "event plain ALL" &
           ASCII.CR & ASCII.LF & ASCII.CR & ASCII.LF);

   Send (Test_Client, "api status" &
           ASCII.CR & ASCII.LF & ASCII.CR & ASCII.LF);

   if not Channel_List (Test_Client).Empty then
      Put (ESL.Client.Tasking.Channel_List (Test_Client).Image);
   end if;
   delay 1.0;

   ESL.Client.Tasking.Shutdown (Client => Test_Client);

end ESL.Client.Tasking.Channel_List_Test;
