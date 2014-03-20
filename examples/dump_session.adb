
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

--  Dumps the output (all events) from a FreeSWITCH ESL session to stdout.

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

with ESL.Parsing_Utilities;
with ESL.Basic_Client;
with ESL.Trace;
with ESL.Packet;
with ESL.Packet_Content_Type;
with ESL;

procedure Dump_Session is
   use ESL;
   use ESL.Basic_Client;
   use ESL.Trace;
   use ESL.Parsing_Utilities;

   procedure Handle_Authentication;

   Client : ESL.Basic_Client.Instance;

   procedure Usage;

   procedure Handle_Authentication is
      use ESL.Packet_Content_Type;
      Current_Packet : ESL.Packet.Instance := ESL.Packet.Empty_Packet;
   begin
      --  Skip until the auth request.
      while Current_Packet.Content_Type /= Auth_Request loop
         Current_Packet := ESL.Parsing_Utilities.Read_Packet (Client.Stream);
      end loop;

      Client.Authenticate (Password => Argument (3));
   end Handle_Authentication;

   procedure Usage is
   begin
      Put_Line ("Usage:" & Command_Name & " hostname port password");
      Put_Line ("Exiting...");
      Set_Exit_Status (Failure);
   end Usage;
begin
   ESL.Trace.Unmute (Every);

   if Argument_Count < 3 then
      Usage;
      return;
   end if;

   Client.Connect (Argument (1), Natural'Value (Argument (2)));
   Handle_Authentication;

   Client.Unmute_Event (Format => Plain,
                        Event  => "all");

   loop
      Put_Line (ESL.Parsing_Utilities.Get_Line (Stream => Client.Stream));
   end loop;
end Dump_Session;
