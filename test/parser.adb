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

   Tests     : constant array (Natural range <>) of access String :=
     (new String'("test_cases/event_minimum"),
      new String'("test_cases/event_channel_answer"),
      new String'("test_cases/event_channel_destroy"),
      new String'("test_cases/event_channel_hangup"),
      new String'("test_cases/event_channel_state"),
      new String'("test_cases/event_channel_create"),
      new String'("test_cases/event_channel_callstate"));

   Test_File : File_Type;

   procedure File_Tests;
   pragma Unreferenced (File_Tests);

   procedure File_Tests is
   begin
      for I in Tests'Range loop

         Open (Test_File, In_File, Tests (I).all);

         while not End_Of_File (Test_File) loop
            declare
               Field : ESL.Packet_Field.Instance;
               pragma Unreferenced (Field);
            begin
               Field := Parse_Line (Get_Line (Test_File));
            exception
               when others =>
                  null;
            end;

         end loop;

         Close (Test_File);
      end loop;
   end File_Tests;

   procedure Test_Session;
   pragma Unreferenced (Test_Session);

   procedure Test_Session is
      Session_File : File_Type;
   begin
      Open (File => Session_File,
            Mode => In_File,
            Name => "test_cases/event_channel_callstate");

      --      while not End_Of_File (Test_File) loop
   end Test_Session;

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
