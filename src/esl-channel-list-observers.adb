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
with ESL.Packet_Keys;

package body ESL.Channel.List.Observers is
   use ESL.Trace;

   procedure Notify (Observer : access State_Observer;
                     Packet   : in     ESL.Packet.Instance;
                     Client   : in     ESL.Client.Reference) is
      Context : constant String := Package_Name & "Nofity (State_Observer)";

   begin

      --  TODO: FIX
--        Client.Channel_List.Change_State
--          (Key       =>
--             Value (Packet.Field (Key => ESL.Packet_Keys.Unique_ID).Value),
--           New_State =>
--             Value (Packet.Field (Key => ESL.Packet_Keys.Channel_State).Value));

      ESL.Trace.Debug (Message => "Triggered",
                       Context => Context);
   end Notify;

   procedure Notify (Observer : access Answer_Observer;
                     Packet   : in     ESL.Packet.Instance;
                     Client   : in     ESL.Client.Reference) is
      Context : constant String := Package_Name & "Nofity (Answer_Observer)";

   begin
      ESL.Trace.Debug (Message => "Triggered",
                       Context => Context);
   end Notify;

   procedure Notify (Observer : access Create_Observer;
                     Packet   : in     ESL.Packet.Instance;
                    Client   : in     ESL.Client.Reference) is
      Context : constant String := Package_Name & "Nofity (Create_Observer)";
      C       : Channel.Instance := Channel.Create (Packet => Packet);
   begin
      ESL.Trace.Information (Message => "Triggered",
                             Context => Context);
      --  TODO: FIX
--        if Client.Channel_List = null then
--           ESL.Trace.Information (Message => "null-list detected!",
--                                  Context => Context);
--        else
--           ESL.Trace.Information (Message => "inserting",
--                                  Context => Context);
--           Client.Channel_List.Insert (C);
--        end if;
--     exception
--        when others =>
--           ESL.Trace.Error (Message => "EXCEPTION!",
--                            Context => Context);

   end Notify;

end ESL.Channel.List.Observers;
