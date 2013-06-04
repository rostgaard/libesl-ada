with Ada.Command_Line;
with Ada.Text_IO;

with ESL.Client.Tasking;
with ESL.Packet_Keys;
with ESL.Client.Tasking.Test_Utilities;
with ESL.Channel.List;
with ESL.Channel.List.Observers;
with ESL;

procedure ESL.Client.Tasking.Channel_List_Test is
   use Ada.Command_Line;
   use Ada.Text_IO;
   use ESL;
   use Client.Tasking.Test_Utilities;

   Client : ESL.Client.Tasking.Instance := Create;

   Testobs1 : Re_Schedule_Observer
     (Observing => Event_Stream (Client => Client,
                                 Stream => ESL.Packet_Keys.RE_SCHEDULE));

   Testobs2 : Heartbeat_Observer
     (Observing => Event_Stream (Client => Client,
                                 Stream => ESL.Packet_Keys.HEARTBEAT));

   CO : ESL.Channel.List.Observers.Create_Observer
     (Observing => Event_Stream (Client => Client,
                                 Stream => ESL.Packet_Keys.CHANNEL_CREATE));

   SO : ESL.Channel.List.Observers.State_Observer
     (Observing => Event_Stream (Client => Client,
                                 Stream => ESL.Packet_Keys.CHANNEL_STATE));

   procedure Usage;

   procedure Usage is
   begin
      Put_Line ("Usage:" & Command_Name & " hostname port");
      Set_Exit_Status (Failure);
   end Usage;

begin

   SO.Channel_List := ESL.Client.Tasking.Test_Utilities.List'Access;
   CO.Channel_List := ESL.Client.Tasking.Test_Utilities.List'Access;

   --  ESL.Trace.Mute (ESL.Trace.Debug) := False;

   if Argument_Count < 2 then
      Usage;
      return;
   end if;

   Connect (Client, Argument (1), Natural'Value (Argument (2)));
   Authenticate (Client, Password => "1234");

   Send (Client, "event plain ALL" &
           ASCII.CR & ASCII.LF & ASCII.CR & ASCII.LF);

   Send (Client, "api status" &
           ASCII.CR & ASCII.LF & ASCII.CR & ASCII.LF);

   loop
      if not List.Empty then
         Put (List.Image);
      end if;
      delay 1.0;
   end loop;

end ESL.Client.Tasking.Channel_List_Test;
