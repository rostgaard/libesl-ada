with Ada.Text_IO; use Ada.Text_IO;

with ESL.Parsing_Utilities;
with ESL.Client;
with ESL.Packet;
with ESL;

procedure Parser is
   use ESL;
   use ESL.Parsing_Utilities;

   Client : ESL.Client.Instance;

begin

   Client.Connect ("localhost", 8021);
   Client.Authenticate (Password => "ClueCon");

   Client.Send ("event plain ALL" & ASCII.CR & ASCII.LF & ASCII.CR & ASCII.LF);

   Client.Send ("api status" &
                  ASCII.CR & ASCII.LF & ASCII.CR & ASCII.LF);

   loop
      declare
         Packet : constant ESL.Packet.Instance :=
           ESL.Parsing_Utilities.Read_Packet (Stream => Client.Stream);
      begin
         Put_Line (Packet.Image);
      end;
   end loop;
end Parser;
