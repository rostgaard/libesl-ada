with Ada.Text_IO; use Ada.Text_IO;
--  with Ada.Exceptions;

with ESL.Packet_Field;
with ESL.Parsing_Utilities;
with ESL.Client;

procedure Parser is
   use ESL.Parsing_Utilities;

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

   procedure Test_Session;

   procedure Test_Session is
      Session_File : File_Type;
   begin
      Open (File => Session_File,
            Mode => In_File,
            Name => "test_cases/event_channel_callstate");

      --      while not End_Of_File (Test_File) loop
   end Test_Session;

   procedure File_Tests;

   procedure File_Tests is
   begin
      for I in Tests'Range loop

         Open (Test_File, In_File, Tests (I).all);

         while not End_Of_File (Test_File) loop
            declare
               use ESL.Packet_Field;
               Field : ESL.Packet_Field.Instance;
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

begin
   Client.Connect ("localhost", 8021);

   Client.Send ("auth ClueCon" & ASCII.CR & ASCII.LF & ASCII.CR & ASCII.LF);
   Client.Send ("event plain ALL" & ASCII.CR & ASCII.LF & ASCII.CR & ASCII.LF);

   Client.Send ("api xml_wrap status" &
                  ASCII.CR & ASCII.LF & ASCII.CR & ASCII.LF);

   loop
      Put_Line (Client.Get_Line);
   end loop;

end Parser;
