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

with Ada.Calendar;

with ESL.UUID;
with ESL.Packet;

package ESL.Job is

   type Instance is tagged private;

   type Actions is (Unknown, Discard, Keep, Aborted, Ready);

   function "=" (Left, Right : in Instance) return Boolean;

   function "<" (Left, Right : in Instance) return Boolean;

   function Create (UUID      : in ESL.UUID.Instance;
                    When_Done : in Actions) return Instance;

   function Image (Job : Instance) return String;

   procedure Set_Packet (Job    :    out Instance;
                         Packet : in     ESL.Packet.Instance);
private
   type Instance is tagged
      record
         Timestamp : Ada.Calendar.Time   := Ada.Calendar.Clock;
         UUID      : ESL.UUID.Instance   := ESL.UUID.Null_UUID;
         Action    : Actions             := Discard;
         Packet    : ESL.Packet.Instance := ESL.Packet.Empty_Packet;
      end record;

end ESL.Job;
