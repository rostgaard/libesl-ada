with Ada.Strings.Fixed; use Ada.Strings.Fixed; -- For Index
with Ada.Strings.Maps;

with ESL.Trace;

package body ESL.Parsing_Utilities is
   function Parse_Line (Item : in String) return ESL.Packet_Field.Instance is
      use ESL.Packet_Field;
      Underscore_Map : constant Ada.Strings.Maps.Character_Mapping
        := Ada.Strings.Maps.To_Mapping ("-", "_");

      Seperator_Index : Natural := Index
        (Source  => Item,
         Pattern => Seperator);
      Key_Length     : constant Natural :=
        Seperator_Index - Seperator'Length - 1;

   begin
      --  Sometimes we get string slice instead of a "real" string.
      if Item'First /= 1 then
         Seperator_Index := Seperator_Index - Item'First + 1;
      end if;

      --  Return the anonymous object
      return Create (Key => Translate
                    (Source  => Item
                     (Item'First .. Item'First + Key_Length),
                     Mapping => Underscore_Map),
                     Value =>
                       Item (Item'First + Seperator_Index + 1 .. Item'Last));
   exception
      when Constraint_Error =>
         ESL.Trace.Information ("Unknown line """ & Item & """");
         raise;
   end Parse_Line;

end ESL.Parsing_Utilities;
