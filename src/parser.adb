with Ada.Text_IO; use Ada.Text_IO;
--  with Ada.Exceptions;
with Ada.Strings.Unbounded;

with ESL.Packet_Field;
with ESL.Parsing_Utilities;
with ESL.Client;
with ESL.Packet_Keys;

procedure Parser is
   use ESL.Parsing_Utilities;
   use Ada.Strings.Unbounded;

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

   procedure Test_Session;

   procedure Test_Session is
      Session_File : File_Type;
   begin
      Open (File => Session_File,
            Mode => In_File,
            Name => "test_cases/event_channel_callstate");

      --      while not End_Of_File (Test_File) loop
   end Test_Session;

begin

   Client.Connect ("localhost", 8021);

   Client.Send ("auth ClueCon" & ASCII.CR & ASCII.LF & ASCII.CR & ASCII.LF);
   Client.Send ("event plain ALL" & ASCII.CR & ASCII.LF & ASCII.CR & ASCII.LF);

   Client.Send ("api status" &
                  ASCII.CR & ASCII.LF & ASCII.CR & ASCII.LF);

   declare
      use ESL.Packet_Field;
      use ESL.Packet_Keys;
      Field : ESL.Packet_Field.Instance;
      Seen_Content_Length : Boolean := False;
      Bytecount : Natural := 0;
      Length    : Natural := 0;
   begin
      loop
         Field := Parse_Line (Client.Get_Line);

         if Field.Key = Content_Length then
            Seen_Content_Length := True;

            Put_Line ("Content_length: " & Field.Value);

            --  Skip until new_line:
            Client.Skip_Until_Empty_Line;

            declare
               Buffer : String (1 .. Natural'Value (Field.Value));
            begin

               Buffer := Client.Receive (Count => Natural'Value (Field.Value));
               New_Line;
               Put ("Buffer start [");
               Put (Buffer);
               Put_Line ("] Buffer end");
               --  Reset values.
               Seen_Content_Length := False;
               Bytecount := 0;
               Length := 0;
            end;
            end if;
         end loop;
      end;

   end Parser;
