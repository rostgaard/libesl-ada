with Ada.Text_IO; use Ada.Text_IO;

with ESL.Packet_Field;
with ESL.Parsing_Utilities;
with ESL.Client;
with ESL.Packet_Keys;
with ESL.Packet;
with ESL;

procedure Parser is
   use ESL.Parsing_Utilities;
   use ESL.Packet_Field;
   use ESL.Packet_Keys;

   Client : ESL.Client.Instance;

begin

   Client.Connect ("localhost", 8021);
   Client.Authenticate (Password => "ClueCon");

   Client.Send ("event plain ALL" & ASCII.CR & ASCII.LF & ASCII.CR & ASCII.LF);

   Client.Send ("api status" &
                  ASCII.CR & ASCII.LF & ASCII.CR & ASCII.LF);

   loop
      declare
         Field  : ESL.Packet_Field.Instance;
         Packet : ESL.Packet.Instance;
      begin

         --  Harvest headers.
         loop
            Field := Parse_Line (Client.Get_Line);

            Packet := ESL.Parsing_Utilities.Read_Packet (Client.Stream);
            Packet.Add_Header (Field);

            exit when Field = Empty_Line;
         end loop;

         if Packet.Has_Header (Event_Keys'(Content_Length)) then
            declare
               Buffer   : String (1 .. Packet.Content_Length);
            begin
               --  Receive the entire buffer.
               Buffer := Client.Receive (Count => Packet.Content_Length);
               Packet.Process_And_Add_Body (Buffer);
            end;
         end if;
      end;
   end loop;

end Parser;
