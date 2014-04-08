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

with Ada.Strings.Fixed;

with ESL.Case_Insensitive_Equal,
     ESL.Case_Insensitive_Less_Than;

package body ESL.UUID is

   function "<" (Left, Right : in Instance) return Boolean is
   begin
      return Case_Insensitive_Less_Than (Left  => To_String (Left.Value),
                                         Right => To_String (Right.Value));
   end "<";

   overriding
   function "=" (Left, Right : in Instance) return Boolean is
   begin
      return Case_Insensitive_Equal (Left  => To_String (Left.Value),
                                     Right => To_String (Right.Value));
   end "=";

   --  TODO: complete.
   function Create (Item : in String) return UUID.Instance is
      use Ada.Strings;
      use Ada.Strings.Fixed;
   begin
      return (Value => To_Unbounded_String (Trim (Source => Item,
                                                  Side   => Both)));
   end Create;

   function Image (UUID : in Instance) return String is
   begin
      if UUID = Null_UUID then
         return "<null>";
      else
         return To_String (UUID.Value);
      end if;
   end Image;

end ESL.UUID;
