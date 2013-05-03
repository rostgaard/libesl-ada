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

with Ada.Strings.Fixed; use Ada.Strings.Fixed; -- For Index
with Ada.Strings.Maps;

with ESL.Trace;

package body ESL.Parsing_Utilities is

   function Dash_To_Underscore (Source : in String) return String is
      Underscore_Map : constant Ada.Strings.Maps.Character_Mapping
        := Ada.Strings.Maps.To_Mapping ("-", "_");
   begin
      return  Translate (Source  => Source,
                         Mapping => Underscore_Map);
   end Dash_To_Underscore;

   function Parse_Line (Item : in String) return ESL.Packet_Variable.Instance
   is
      use ESL.Packet_Variable;
      use ESL.Packet_Field;

      Context : constant String := Package_Name & ".Parse_Line (Variable)";

      Seperator_Position : constant Natural := Index
        (Source  => Item,
         Pattern => ESL.Packet_Field.Seperator);

   begin
      ESL.Trace.Debug (Message => Item,
                       Context => Context);

      if Item'Length = 0 then
         return Packet_Variable.Empty_Line;
      end if;

      --  Return the anonymous object
      return Create (Name          =>
                       Item (Item'First .. Seperator_Position),
                     Initial_Value =>
                       Item (Seperator_Position +
                           Seperator'Length .. Item'Last));
   exception
      when Constraint_Error =>
         ESL.Trace.Information (Message => "Unknown line """ & Item & """",
                                Context => Context);
         raise;
   end Parse_Line;

   ------------------
   --  Parse_Line  --
   ------------------

   function Parse_Line (Item : in String) return ESL.Packet_Field.Instance is
      use ESL.Packet_Field;

      Context : constant String := Package_Name & ".Parse_Line (Field)";

      Seperator_Index : Natural := Index
        (Source  => Item,
         Pattern => Seperator);
      Key_Length     : constant Integer :=
        Seperator_Index - Seperator'Length - 1;

   begin
      if Item'Length = 0 then
         return Empty_Line;
      end if;

      --  Sometimes we get string slice instead of a "real" string.
      if Item'First /= 1 then
         Seperator_Index := Seperator_Index - Item'First + 1;
      end if;

      --  Return the anonymous object
      return Create (Key => Dash_To_Underscore (
                     Source  => Item
                       (Item'First .. Item'First + Key_Length)),
                     Value =>
                       Item (Item'First + Seperator_Index + 1 .. Item'Last));
   exception
      when Constraint_Error =>
         ESL.Trace.Information (Message => "Unknown line """ & Item & """",
                                Context => Context);
         return Unknown_Line;
   end Parse_Line;

   function Underscore_To_Dash (Source : in String) return String is
      Underscore_Map : constant Ada.Strings.Maps.Character_Mapping
        := Ada.Strings.Maps.To_Mapping ("_", "-");
   begin
      return  Translate (Source  => Source,
                         Mapping => Underscore_Map);
   end Underscore_To_Dash;

end ESL.Parsing_Utilities;
