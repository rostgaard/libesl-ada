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
with Ada.Strings;
with Ada.Integer_Text_IO;

package body Hex_Utilities is
   ---------
   -- Hex --
   ---------

   function Hex (V : Natural; Width : Natural := 0) return String is
      use Ada.Strings;
      Hex_V : String (1 .. Integer'Size / 4 + 4);
   begin
      Ada.Integer_Text_IO.Put (Hex_V, V, 16);

      declare
         Result : constant String
           := Hex_V (Fixed.Index (Hex_V, "#") + 1
                       .. Fixed.Index (Hex_V, "#", Backward) - 1);
      begin
         if Width = 0 then
            return Result;

         elsif Result'Length < Width then
            declare
               use Ada.Strings.Fixed;
               Zero : constant String := (Width - Result'Length) * '0';
            begin
               return Zero & Result;
            end;

         else
            return Result (Result'Last - Width + 1 .. Result'Last);
         end if;
      end;
   end Hex;

   ---------------
   -- Hex_Value --
   ---------------

   function Hex_Value (Hex : String) return Natural is

      function Value (C : Character) return Natural;
      pragma Inline (Value);
      --  Return value for single character C

      -----------
      -- Value --
      -----------

      function Value (C : Character) return Natural is
      begin
         case C is
            when '0'       => return 0;
            when '1'       => return 1;
            when '2'       => return 2;
            when '3'       => return 3;
            when '4'       => return 4;
            when '5'       => return 5;
            when '6'       => return 6;
            when '7'       => return 7;
            when '8'       => return 8;
            when '9'       => return 9;
            when 'a' | 'A' => return 10;
            when 'b' | 'B' => return 11;
            when 'c' | 'C' => return 12;
            when 'd' | 'D' => return 13;
            when 'e' | 'E' => return 14;
            when 'f' | 'F' => return 15;
            when others    => raise Constraint_Error;
         end case;
      end Value;

      R : Natural := 0;

   begin
      for K in Hex'Range loop
         R := R * 16 + Value (Hex (K));
      end loop;

      return R;
   end Hex_Value;
end Hex_Utilities;
