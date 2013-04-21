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

package body ESL.Packet_Field is

   function Create (Key   : in String;
                    Value : in String) return Instance is
   begin
      return Create (Key   => Event_Keys'Value (Key),
                     Value => Value);
   end Create;

   function Create (Key   : in Event_Keys;
                    Value : in String) return Instance is
   begin
      return  (Key   => Key,
               Value => To_Unbounded_String (Value));
   end Create;

   function Image (Item : in Instance) return String is
   begin
      if Item /= Empty_Line then
         return Item.Key'Img & Seperator & To_String (Item.Value);
      else
         return "";
      end if;
   end Image;

   function Key (Obj : in Instance) return ESL.Packet_Keys.Event_Keys is
   begin
      return Obj.Key;
   end Key;

   function Value (Obj : in Instance) return String is
   begin
      return To_String (Obj.Value);
   end Value;

end ESL.Packet_Field;
