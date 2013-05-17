with ESL.Trace;

package body ESL.Client.Tasking.Test_Utilities is

   procedure Notify (Observer : access Heartbeat_Observer;
                     Packet   : in     ESL.Packet.Instance;
                     Client   : in     ESL.Client.Reference) is
      Context : constant String := Package_Name &
        ".Notify (Heartbeat_Observer)";
   begin
      ESL.Trace.Information
        (Message => "Triggered!",
         Context => Context);
   end Notify;

   procedure Notify (Observer : access Re_Schedule_Observer;
                     Packet   : in     ESL.Packet.Instance;
                     Client   : in     ESL.Client.Reference) is
      Context : constant String := Package_Name &
        ".Notify (Re_Schedule_Observer)";
   begin
      ESL.Trace.Information
        (Message => "Triggered!",
         Context => Context);
   end Notify;

end ESL.Client.Tasking.Test_Utilities;
