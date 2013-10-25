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

with Ada.Text_IO; use Ada.Text_IO;

with ESL.Parsing_Utilities;
with ESL.Client.Tasking;
with ESL.Packet;
with ESL;

procedure Parser is
   use ESL;
   use ESL.Parsing_Utilities;

   Client : ESL.Client.Tasking.Instance
     (On_Connect_Handler => ESL.Client.Ignore_Event,
      On_Disconnect_Handler => ESL.Client.Ignore_Event);
   Count  : Natural := 0;
begin

   Client.Connect ("responsum.pbx.jay.net", 8021);
   Client.Authenticate (Password => "1234");

   Client.Send ("event json all" & ASCII.CR & ASCII.LF & ASCII.CR & ASCII.LF);

   Client.Send ("api status" &
                  ASCII.CR & ASCII.LF & ASCII.CR & ASCII.LF);

   loop
      declare
         Packet : constant ESL.Packet.Instance :=
           ESL.Parsing_Utilities.Read_Packet (Stream => Client.Stream);
      begin
         Count := Count + 1;
         Put_Line (Packet.Content_Type'Img & Count'Img);
      end;
   end loop;
end Parser;
