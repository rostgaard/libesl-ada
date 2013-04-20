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
