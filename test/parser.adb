with Ada.Text_IO; use Ada.Text_IO;

with ESL.Parsing_Utilities;
with ESL.Client;
with ESL.Packet;
with ESL;

procedure Parser is
   use ESL;
   use ESL.Parsing_Utilities;

   Client : ESL.Client.Instance;
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
