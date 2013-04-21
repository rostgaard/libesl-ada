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
--  This file originates from AWS.URL

with Ada.Strings.Maps;

package URL_Utilities is

   --
   --  URL Encoding and Decoding
   --

   Default_Encoding_Set : constant Ada.Strings.Maps.Character_Set;

   function Encode
     (Str          : String;
      Encoding_Set : Ada.Strings.Maps.Character_Set := Default_Encoding_Set)
      return String;
   --  Encode Str into a URL-safe form. Many characters are forbiden into an
   --  URL and needs to be encoded. A character is encoded by %XY where XY is
   --  the character's ASCII hexadecimal code. For example a space is encoded
   --  as %20.

   function Decode (Str : String) return String;
   --  This is the opposite of Encode above

private

   use type Ada.Strings.Maps.Character_Set;

   Default_Encoding_Set : constant Ada.Strings.Maps.Character_Set
     := Ada.Strings.Maps.To_Set
         (Span => (Low  => Character'Val (128),
                   High => Character'Val (Character'Pos (Character'Last))))
     or
       Ada.Strings.Maps.To_Set (";/?:@&=+$,<>#%""{}|\^[]`' ");

end URL_Utilities;
