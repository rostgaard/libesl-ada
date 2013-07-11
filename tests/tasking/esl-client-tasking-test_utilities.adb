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
      ESL.Trace.Information
        (Message => Packet.Image,
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
