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
with URL_Utilities;

package body ESL.Packet_Field is

   -----------
   --  "="  --
   -----------

   function "=" (Left  : in Instance;
                 Right : in Instance) return Boolean is
   begin
      return Left.Key = Right.Key and
        Left.Value = Right.Value;
   end "=";

   --------------
   --  Create  --
   --------------

   function Create (Key   : in String;
                    Value : in String) return Instance is
      New_Key : Event_Keys := Unknown;
   begin
      --  Special cases.
      if Key = "name" then
         New_Key := X_Name;
      elsif Key = "description" then
         New_Key := X_Description;
      elsif Key = "type" then
         New_Key := X_Type;
      elsif Key = "syntax" then
         New_Key := X_Syntax;
      else
         New_Key := Event_Keys'Value (Key);
      end if;

      return Create (Key   => New_Key,
                     Value => Value);
   end Create;

   --------------
   --  Create  --
   --------------

   function Create (Key   : in Event_Keys;
                    Value : in String) return Instance is
   begin
      return  (Key   => Key,
               Value => To_Unbounded_String (Value));
   end Create;

   ---------------------
   --  Decoded_Value  --
   ---------------------

   function Decoded_Value (Obj : in Instance) return String is
   begin
      return URL_Utilities.Decode (To_String (Obj.Value));
   end Decoded_Value;

   -------------
   --  Image  --
   -------------

   function Image (Item : in Instance) return String is
   begin
      if Item /= Empty_Line then
         return Item.Key'Img & Seperator & To_String (Item.Value);
      else
         return "";
      end if;
   end Image;

   -----------
   --  Key  --
   -----------

   function Key (Obj : in Instance) return ESL.Packet_Keys.Event_Keys is
   begin
      return Obj.Key;
   end Key;

   -------------
   --  Value  --
   -------------

   function Value (Obj : in Instance) return String is
   begin
      return To_String (Obj.Value);
   end Value;

end ESL.Packet_Field;
