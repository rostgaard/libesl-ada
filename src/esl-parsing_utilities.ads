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

with Ada.Streams;

with ESL.Packet;
with ESL.Packet_Field;
with ESL.Packet_Variable;
with ESL.Header_Field;

package ESL.Parsing_Utilities is

   Package_Name : constant String := "ESL.Parsing_Utilities";

   function Get_Line (Stream : access Ada.Streams.Root_Stream_Type'Class)
                      return String;

   function Parse_Line (Item : in String) return ESL.Packet_Field.Instance;

   function Parse_Line (Item : in String) return ESL.Packet_Variable.Instance;

   function Parse_Line (Item : in String) return ESL.Header_Field.Instance;

   function Read_Packet (Stream : access Ada.Streams.Root_Stream_Type'Class)
     return ESL.Packet.Instance;

   function Dash_To_Underscore (Source : in String) return String;

   function Underscore_To_Dash (Source : in String) return String;

end ESL.Parsing_Utilities;
