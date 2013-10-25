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

private with Ada.Strings.Unbounded;

package ESL.UUID is
   type Instance is tagged private;

   function Create (Item : in String) return UUID.Instance;

   function Image (UUID : in Instance) return String;

   function Serialize (UUID : in Instance) return String renames Image;

   function "=" (Left, Right : in Instance) return Boolean;

   function "<" (Left, Right : in Instance) return Boolean;

   Null_UUID : constant UUID.Instance;

private
   use Ada.Strings.Unbounded;

   type Instance is tagged
      record
         Value : Unbounded_String := Null_Unbounded_String;
      end record;

   Null_UUID : constant UUID.Instance := (Value => Null_Unbounded_String);

end ESL.UUID;
