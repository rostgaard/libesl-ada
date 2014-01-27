
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

with Ada.Assertions;
with Ada.Text_IO;
with Ada.Streams.Stream_IO;
with Ada.Command_Line;

--  with ESL.Trace;
with ESL.Parsing_Utilities;
with ESL.Packet;

with ESL.Trace;
with ESL.UUID;
with ESL.Job.List;

package body ESL.Job.List.Test is
   use Ada.Assertions;
   use ESL.Trace;
   use ESL;
   use ESL.Parsing_Utilities;
   use Ada.Text_IO;
   use Ada.Streams.Stream_IO;

   Test_File  : Ada.Streams.Stream_IO.File_Type;
   Stream     : Ada.Streams.Stream_IO.Stream_Access;
   Packet     : ESL.Packet.Instance;

   Job     : ESL.Job.Instance;
   UUID    : constant ESL.UUID.Instance := ESL.UUID.Create
     (Item => "17186fe1-2177-4ab8-ac20-b0cbdfd45353");

   procedure Set_Up (T : in out Instance) is
   begin
      ESL.Trace.Unmute (Every);

      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);
      Open (File => Test_File,
            Mode => In_File,
            Name => "./tests/test_cases/event_background_job");

      Stream := Ada.Streams.Stream_IO.Stream (File => Test_File);

      while not Ada.Streams.Stream_IO.End_Of_File (Test_File) loop
         Packet := ESL.Parsing_Utilities.Read_Packet (Stream);
      end loop;

   end Set_Up;

   procedure Early_Pop is
      List    : ESL.Job.List.Instance;
   begin
      List.Pop (UUID => UUID, Job => Job);
      raise Assertion_Error with "Expected exception in previous call.";
   exception
      when ESL.Job.List.Not_Done =>
         null; --  We're good, this is supposed to happen.
   end Early_Pop;

   procedure Resubscribe_Test is
      List    : ESL.Job.List.Instance;
   begin
      List.Subscribe (UUID => UUID);
      begin
         List.Subscribe (UUID => UUID);
         raise Assertion_Error with "Expected exception in previous call.";
      exception
         when Constraint_Error =>
            null; --  We're good, this is supposed to happen.
      end;
   end Resubscribe_Test;

   procedure Timeout_Test_No_Reply is
      List    : ESL.Job.List.Instance;
   begin
      List.Wait_For (UUID => UUID, Job => Job, Timeout => 0.1);
   exception
      when ESL.Job.List.Timeout_Reached =>
         null; --  We're good, this is supposed to happen.
   end Timeout_Test_No_Reply;

   procedure Timeout_Test_Reply is
      List    : ESL.Job.List.Instance;

      task Packet_Pusher is
         entry Start;
      end Packet_Pusher;

      task body Packet_Pusher is
      begin
         accept Start do
            delay 0.1;
            List.Push (Packet => Packet);
         end Start;
      end Packet_Pusher;

   begin
      List.Subscribe (UUID => UUID);
      Put_Line (List.Image);
      Packet_Pusher.Start;

      List.Wait_For (UUID => UUID, Job => Job, Timeout => 0.5);
   end Timeout_Test_Reply;

   procedure Initialize (T : in out Instance) is
   begin
      Set_Name (T, Package_Name);

      Ahven.Framework.Add_Test_Routine
        (T, Early_Pop'Access, "Early_Pop");
      Ahven.Framework.Add_Test_Routine
        (T, Resubscribe_Test'Access, "Resubscribe_Test");
      Ahven.Framework.Add_Test_Routine
        (T, Timeout_Test_No_Reply'Access, "Timeout test (no reply)");
--        Ahven.Framework.Add_Test_Routine
--          (T, Timeout_Test_Reply'Access, "Timeout test (with timely reply)");
   end Initialize;
end ESL.Job.List.Test;
