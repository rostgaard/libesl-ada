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

--  Dumps the output from a FreeSWITCH ESL session to stdout.

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

with ESL.Parsing_Utilities;
with ESL.Client;
with ESL.Trace;
with ESL;

procedure Dump_Session is
   use ESL;
   use ESL.Trace;
   use ESL.Parsing_Utilities;

   Client : ESL.Client.Instance;

   procedure Usage;

   procedure Usage is
   begin
      Put_Line ("Usage:" & Command_Name & " hostname port password");
      Set_Exit_Status (Failure);
   end Usage;
begin
   ESL.Trace.Mute (Every);

   if Argument_Count < 3 then
      Usage;
      return;
   end if;

   Client.Connect (Argument (1), Natural'Value (Argument (2)));
   Client.Authenticate (Password => Argument (3));

   Client.Send ("event plain all");

   loop
      Put_Line (ESL.Parsing_Utilities.Get_Line (Stream => Client.Stream));
   end loop;
end Dump_Session;
