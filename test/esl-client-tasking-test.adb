
with ESL.Client.Tasking;
with ESL.Packet_Keys;
with ESL.Client.Tasking.Test_Utilities;

with ESL;

procedure ESL.Client.Tasking.Test is
   use ESL;
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

   Connect (Client, "localhost", 8021);
   Authenticate (Client, Password => "ClueCon");

   Send (Client, "event plain ALL" &
           ASCII.CR & ASCII.LF & ASCII.CR & ASCII.LF);

   Send (Client, "api status" &
                  ASCII.CR & ASCII.LF & ASCII.CR & ASCII.LF);
   delay 10.0;
end ESL.Client.Tasking.Test;
