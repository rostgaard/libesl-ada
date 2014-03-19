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

with Ada.Characters.Handling;

with Hex_Utilities;

package body URL_Utilities is
   subtype Escape_Code is String (1 .. 2);

   Not_Escaped : constant Escape_Code := "  ";

   function Code (C : Character) return Escape_Code;
   pragma Inline (Code);
   --  Returns hexadecimal code for character C

   subtype ASCII_7 is Character range Character'First .. Character'Val (127);
   type ASCII_7_Set is array (ASCII_7) of Escape_Code;

   function Build_Hex_Escape return ASCII_7_Set;
   --  Returns the table with pre-computed encoding for 7bits characters

   ----------------------
   -- Build_Hex_Escape --
   ----------------------

   function Build_Hex_Escape return ASCII_7_Set is
      Result : ASCII_7_Set;
   begin
      for C in Character'Val (0) .. Character'Val (127) loop
         if Ada.Strings.Maps.Is_In (C, Default_Encoding_Set) then
            Result (C) := Code (C);
         else
            Result (C) := Not_Escaped;
         end if;
      end loop;
      return Result;
   end Build_Hex_Escape;

   ----------
   -- Code --
   ----------

   function Code (C : Character) return Escape_Code is
   begin
      return Hex_Utilities.Hex (Character'Pos (C));
   end Code;

   Hex_Escape : constant ASCII_7_Set :=  Build_Hex_Escape;
   --  Limit Hex_Escape to 7bits ASCII characters only. Other ISO-8859-1 are
   --  handled separately in Encode function. Space character is not processed
   --  specifically, contrary to what is done in AWS.URL.

   ------------
   -- Decode --
   ------------

   function Decode (Str : String) return String is
      Res : String (1 .. Str'Length);
      K   : Natural := 0;
      I   : Positive := Str'First;
   begin
      if Str = "" then
         return "";
      end if;

      loop
         K := K + 1;

         if Str (I) = '%'
           and then I + 2 <= Str'Last
           and then Ada.Characters.Handling.Is_Hexadecimal_Digit (Str (I + 1))
           and then Ada.Characters.Handling.Is_Hexadecimal_Digit (Str (I + 2))
         then
            Res (K) := Character'Val
              (Hex_Utilities.Hex_Value (Str (I + 1 .. I + 2)));
            I := I + 2;

         elsif Str (I) = '+' then
            --  A plus is used for spaces in forms value for example
            Res (K) := ' ';

         else
            Res (K) := Str (I);
         end if;

         I := I + 1;
         exit when I > Str'Last;
      end loop;

      return Res (1 .. K);
   end Decode;

   ------------
   -- Encode --
   ------------

   function Encode
     (Str          : String;
      Encoding_Set : Ada.Strings.Maps.Character_Set := Default_Encoding_Set)
      return String
   is
      C_128 : constant Character := Character'Val (128);
      Res   : String (1 .. Str'Length * 3);
      K     : Natural := 0;
   begin
      for I in Str'Range loop
         if Ada.Strings.Maps.Is_In (Str (I), Encoding_Set) then
            --  This character must be encoded

            K := K + 1;
            Res (K) := '%';
            K := K + 1;

            if Str (I) < C_128 then
               --  We keep a table for characters lower than 128 for efficiency
               Res (K .. K + 1) := Hex_Escape (Str (I));
            else
               Res (K .. K + 1) := Code (Str (I));
            end if;

            K := K + 1;

         else
            K := K + 1;
            Res (K) := Str (I);
         end if;
      end loop;

      return Res (1 .. K);
   end Encode;

end URL_Utilities;
