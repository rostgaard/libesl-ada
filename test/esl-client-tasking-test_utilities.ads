with ESL.Observer.Event_Observers;
with ESL.Packet;

package ESL.Client.Tasking.Test_Utilities is

   Package_Name : constant String := "ESL.Client.Tasking.Test_Utilities";

   type Heartbeat_Observer is
     new ESL.Observer.Event_Observers.Instance with null record;

   overriding
   procedure Notify (Observer : access Heartbeat_Observer;
                     Packet   : in     ESL.Packet.Instance;
                     Client   : in     ESL.Client.Reference);

   type Re_Schedule_Observer is
     new ESL.Observer.Event_Observers.Instance with null record;

   overriding
   procedure Notify (Observer : access Re_Schedule_Observer;
                     Packet   : in     ESL.Packet.Instance;
                     Client   : in     ESL.Client.Reference);
end ESL.Client.Tasking.Test_Utilities;
