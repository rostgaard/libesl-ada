--  This file originates from AWS.Utils.

package Hex_Utilites is

   function Hex (V : Natural; Width : Natural := 0) return String;
   --  Returns the hexadecimal string representation of the decimal
   --  number V. if Width /= 0, the result will have exactly Width characters
   --  eventually padded with leading 0 or trimmed on the right.

   function Hex_Value (Hex : String) return Natural;
   --  Returns the value for the hexadecimal number Hex. Raises
   --  Constraint_Error is Hex is not an hexadecimal number.

end Hex_Utilites;
