--  Dumps the output from a FreeSWITCH ESL session to stdout.

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

with ESL.Parsing_Utilities;
with ESL.Client;
with ESL.Trace;
with ESL;

procedure Dump_Session is
   use ESL;
   use ESL.Trace;
   use ESL.Parsing_Utilities;

   Client : ESL.Client.Instance;

   procedure Usage;

   procedure Usage is
   begin
      Put_Line ("Usage:" & Command_Name & " hostname port password");
      Set_Exit_Status (Failure);
   end Usage;
begin
   ESL.Trace.Mute (Every);

   if Argument_Count < 3 then
      Usage;
      return;
   end if;

   Client.Connect (Argument (1), Natural'Value (Argument (2)));
   Client.Authenticate (Password => Argument (3));

   Client.Send ("event plain all");

   loop
      Put_Line (ESL.Parsing_Utilities.Get_Line (Stream => Client.Stream));
   end loop;
end Dump_Session;
