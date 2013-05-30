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

--  with Ada.Assertions;
with Ada.Calendar;
with Ada.Text_IO;
with Ada.Streams.Stream_IO;

with ESL.Trace;
with ESL.Parsing_Utilities;
with ESL.Packet;

procedure ESL.Packet.Test is
   use Ada.Calendar;
   use ESL;
   use ESL.Trace;
   use ESL.Parsing_Utilities;
   use ESL.Packet_Field;
   use ESL.Packet_Keys;
   use Ada.Text_IO;

   Tests     : constant array (Natural range <>) of access String :=
     (new String'("test_cases/basic_session"),
      new String'("test_cases/event_channel_hangup_complete"),
      new String'("test_cases/call_session2"),
      new String'("test_cases/event_channel_destroy"),
      new String'("test_cases/event_channel_hangup"),
      new String'("test_cases/event_channel_state"),
--      new String'("test_cases/event_channel_create"),
      new String'("test_cases/event_channel_callstate"));

   Test_File         : Ada.Streams.Stream_IO.File_Type;
   File_Tests_Errors : Natural    := 0;

   procedure File_Tests;

   procedure File_Tests is
      Stream  : Ada.Streams.Stream_IO.Stream_Access;
      Packet  : ESL.Packet.Instance;
      Count   : Natural := 0;
      Time    : Ada.Calendar.Time;
      Runtime : Duration := 0.0;
      Tmp     : Duration := 0.0;
   begin
      Time := Ada.Calendar.Clock;
      for I in 1 .. 1 loop
         for I in Tests'Range loop
            --Put_Line ("Testing file " & Tests (I).all);
            Ada.Streams.Stream_IO.Open
              (File => Test_File,
               Name => Tests (I).all,
               Mode => Ada.Streams.Stream_IO.In_File);

            Stream := Ada.Streams.Stream_IO.Stream (File => Test_File);

            while not Ada.Streams.Stream_IO.End_Of_File (Test_File) loop
               begin
                  Packet := ESL.Parsing_Utilities.Read_Packet (Stream);
               exception
                  when others =>
                     Put_Line ("Error in file " & Tests (I).all);
                     File_Tests_Errors := File_Tests_Errors + 1;
                     raise;
               end;
               Count := Count+1;
               --            New_Line;
               --            Put_Line ("Packet contents:");
               --            Put_Line (Packet.Image);
               --            New_Line;
               Packet := ESL.Packet.Create;
            end loop;

            --           File_Tests_Errors := 0;
            --
         --           while not End_Of_File (Test_File) loop
         --              declare
         --                 Packet : ESL.Packet.Instance;
         --                 pragma Unreferenced (Field);
         --              begin
         --                 Field := Parse_Line (Get_Line (Test_File));
         --              exception
         --                 when others =>
         --                    File_Tests_Errors := File_Tests_Errors + 1;
         --              end;
         --           end loop;

--           Ada.Assertions.Assert (Check   => File_Tests_Errors = 0,
--                                  Message => "File " & Tests (I).all &
--                                    " failed with" &
--                                    File_Tests_Errors'Img & " errors.");
            Ada.Streams.Stream_IO.Close (Test_File);
         end loop;
      end loop;

      Runtime := Ada.Calendar.Clock - Time;
      Tmp := Duration (Float (Count))/Runtime;
      Put_Line ("Processed" & Count'Img & " packages, with" & File_Tests_Errors'Img & " errors in" & Runtime'Img & " seconds (" & Tmp'Img & " pkts/s).");
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

begin

   --ESL.Trace.Mute (Information) := True;
   --ESL.Trace.Mute (Error) := True;
   File_Tests;

end ESL.Packet.Test;
