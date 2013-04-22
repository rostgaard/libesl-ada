with Ada.Text_IO; use Ada.Text_IO;

with ESL.Packet_Field;
with ESL.Parsing_Utilities;
with ESL.Client;
with ESL.Packet_Keys;
with ESL.Packet;

procedure Parser is
   use ESL.Parsing_Utilities;
   use ESL.Packet_Field;
   use ESL.Packet_Keys;
   Field  : ESL.Packet_Field.Instance;
   Packet : ESL.Packet.Instance;

   Client : ESL.Client.Instance;

   End_Packet_String : constant String :=
     ASCII.CR & ASCII.LF & ASCII.CR & ASCII.LF;

begin

   Client.Connect ("localhost", 8021);

   Client.Send ("auth ClueCon" & End_Packet_String);
   Client.Send ("event plain ALL" & ASCII.CR & ASCII.LF & ASCII.CR & ASCII.LF);

   Client.Send ("api status" &
                  ASCII.CR & ASCII.LF & ASCII.CR & ASCII.LF);

   loop

      --  Harvest headers.
      loop
         Field := Parse_Line (Client.Get_Line);
         Packet.Add_Header (Field);

         exit when Field = Empty_Line;
      end loop;

      if Packet.Has_Header (Content_Length) then
         declare
            Buffer   : String (1 .. Packet.Content_Length);
         begin
            --  Receive the entire buffer.
            Buffer := Client.Receive (Count => Packet.Content_Length);

            Packet.Process_And_Add_Body (Buffer);
         end;
      end if;
      Put_Line (Packet.Image);
   end loop;

end Parser;
