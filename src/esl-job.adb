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

with Ada.Calendar.Formatting;

package body ESL.Job is
   use ESL.UUID;

   function "<" (Left, Right : Instance) return Boolean is
      use Ada.Calendar;
   begin
      if Left.Timestamp = Right.Timestamp then
         return Left.UUID < Right.UUID;
      else
         return Left.Timestamp < Right.Timestamp;
      end if;
   end "<";

   overriding
   function "=" (Left, Right : Instance) return Boolean is
      use Ada.Calendar;
   begin
      return (Left.UUID = Right.UUID) and (Left.Timestamp = Right.Timestamp);
   end "=";

   function Create (UUID      : in ESL.UUID.Instance;
                    When_Done : in Actions) return Instance is
   begin
      return (Timestamp => Ada.Calendar.Clock,
              Action    => When_Done,
              UUID      => UUID,
              Packet    => ESL.Packet.Empty_Packet);
   end Create;

   function Image (Job : Instance) return String is
   begin
      return
        "Submitted: " & Ada.Calendar.Formatting.Image (Job.Timestamp) &
        " UUID: "     & Job.UUID.Image &
        " Action: "   & Job.Action'Img;
   end Image;

   procedure Set_Packet (Job    :    out Instance;
                         Packet : in     ESL.Packet.Instance) is
   begin
      Job.Packet := Packet;
   end Set_Packet;
end ESL.Job;
