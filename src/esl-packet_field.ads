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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with ESL.Packet_Keys;

package ESL.Packet_Field is
   use ESL.Packet_Keys;

   Seperator : constant String := ":";

   type Instance is tagged private;

   Empty_Line   : constant Instance;
   Unknown_Line : constant Instance;

   function Create (Key   : in String;
                    Value : in String) return Instance;

   function Create (Key   : in Event_Keys;
                    Value : in String) return Instance;

   function Image (Item : in Instance) return String;

   function Key (Obj : in Instance) return ESL.Packet_Keys.Event_Keys;

   function Value (Obj : in Instance) return String;

private
   type Instance is tagged
      record
         Key   : Event_Keys;
         Value : Unbounded_String;
      end record;

   Empty_Line   : constant Instance := (Key   => Event_Keys '(Unknown),
                                        Value => Null_Unbounded_String);

   Unknown_Line : constant Instance :=
     (Key   => Event_Keys '(Unknown),
      Value => To_Unbounded_String ("Uknown"));
end ESL.Packet_Field;
