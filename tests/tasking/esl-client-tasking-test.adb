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

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;

with ESL.Client.Tasking;
with ESL.Packet_Keys;
with ESL.Client.Tasking.Test_Utilities;

with ESL;
with ESL.Trace;
with ESL.Command.Call_Management;

procedure ESL.Client.Tasking.Test is
   use ESL;
   use ESL.Trace;
   use Client.Tasking.Test_Utilities;

   Client : ESL.Client.Tasking.Instance;

   Testobs1 : Re_Schedule_Observer
     (Observing => Event_Stream (Client => Client,
                                 Stream => ESL.Packet_Keys.RE_SCHEDULE));

   Testobs2 : Heartbeat_Observer
     (Observing => Event_Stream (Client => Client,
                                 Stream => ESL.Packet_Keys.HEARTBEAT));

   Command : ESL.Command.Call_Management.Instance :=
     ESL.Command.Call_Management.Originate
       (Call_URL         => "user/1001",
        Extension        => "5900");

   procedure Usage;

   procedure Usage is
   begin
      Put_Line ("Usage:" & Command_Name & " hostname port password");
      Set_Exit_Status (Failure);
   end Usage;

begin
   --  ESL.Trace.Mute (ESL.Trace.Debug) := False;

   if Argument_Count < 3 then
      Usage;
      --  TODO: STOP the task.
      return;
   end if;

   Connect (Client, Argument (1), Natural'Value (Argument (2)));
   Authenticate (Client, Password => Argument (3));

   Send (Client, "event plain ALL");

   Send (Client, "api status");
   Client.Send (String (Command.Serialize));

end ESL.Client.Tasking.Test;
