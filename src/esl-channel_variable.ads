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
with Ada.Strings.Unbounded;

package ESL.Channel_Variable is
   use Ada.Strings.Unbounded;

   Package_Name : constant String := "ESL.Channel_Variable";

   type Instance is tagged private;

   overriding
   function "=" (Left, Right : in Instance) return Boolean;

   function Create (Name          : in String;
                    Initial_Value : in String := "") return Instance;

   function Name (Obj : in Instance) return String;

   function Name (Obj : in Instance) return Unbounded_String;

   function Value (Obj : in Instance) return String;

   function Image (Obj : in Instance) return String;

   Empty_Line   : constant Instance;
   Unknown_Line : constant Instance;
private

   type Instance is tagged
      record
         Name  : Unbounded_String;
         Value : Unbounded_String;
      end record;

   Empty_Line  : constant Instance :=
     (Name  => Null_Unbounded_String,
      Value => Null_Unbounded_String);

   Unknown_Line  : constant Instance :=
     (Name  => Null_Unbounded_String,
      Value => To_Unbounded_String ("Unknown"));
end ESL.Channel_Variable;
