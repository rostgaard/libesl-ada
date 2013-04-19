with ESL.Packet_Field;

package ESL.Parsing_Utilities is

   function Parse_Line (Item : in String) return ESL.Packet_Field.Instance;

   function Dash_To_Underscore (Source : in String) return String;

   function Underscore_To_Dash (Source : in String) return String;

end ESL.Parsing_Utilities;
