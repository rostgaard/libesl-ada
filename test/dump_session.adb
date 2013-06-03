--  Dumps the output from a FreeSWITCH ESL session to stdout.

with Ada.Text_IO; use Ada.Text_IO;

with ESL.Parsing_Utilities;
with ESL.Client;
with ESL.Trace;
with ESL.Packet;
with ESL;

procedure Dump_Session is
   use ESL;
   use ESL.Trace;
   use ESL.Parsing_Utilities;

   Client : ESL.Client.Instance;
   Count  : Natural := 0;
begin

   ESL.Trace.Mute (Every);

   Client.Connect ("responsum.pbx.jay.net", 8021);
   Client.Authenticate (Password => "1234");

   Client.Send ("event plain all" & ASCII.CR & ASCII.LF & ASCII.CR & ASCII.LF);

   Client.Send ("api status" &
                  ASCII.CR & ASCII.LF & ASCII.CR & ASCII.LF);

   loop
      Put_Line (ESL.Parsing_Utilities.Get_Line (Stream => Client.Stream));
   end loop;
end Dump_Session;
