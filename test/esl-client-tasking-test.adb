
with ESL.Client.Tasking;
with ESL.Packet_Keys;
with ESL.Client.Tasking.Test_Utilities;

with ESL;
with ESL.Trace;

procedure ESL.Client.Tasking.Test is
   use ESL;
   use ESL.Trace;
   use Client.Tasking.Test_Utilities;

   Client : ESL.Client.Tasking.Instance := Create;

   Testobs1 : Re_Schedule_Observer
     (Observing => Event_Stream (Client => Client,
                                 Stream => ESL.Packet_Keys.RE_SCHEDULE));

   Testobs2 : Heartbeat_Observer
     (Observing => Event_Stream (Client => Client,
                                 Stream => ESL.Packet_Keys.HEARTBEAT));
begin
   --  ESL.Trace.Mute (ESL.Trace.Debug) := False;

   Connect (Client, "responsum.pbx.jay.net", 8021);
   Authenticate (Client, Password => "1234");

   Send (Client, "event json ALL" &
           ASCII.CR & ASCII.LF & ASCII.CR & ASCII.LF);

   Send (Client, "api status" &
                  ASCII.CR & ASCII.LF & ASCII.CR & ASCII.LF);
end ESL.Client.Tasking.Test;
