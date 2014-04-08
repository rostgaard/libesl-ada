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
with Ada.Strings.Maps.Constants;

with ESL.Trace;
with ESL.Parsing_Utilities;

package body ESL.Packet_Content_Type is

   Seperator : constant String := "/";

   function Value (Item : in String) return Types;
   function Value (Item : in String) return Subtypes;
   --  Conversion wrappers.

   overriding
   function "=" (Left  : in Composite;
                 Right : in Composite) return Boolean is
   begin
      return
        (Left.Base = Right.Base) and
        (Left.Sub  = Right.Sub);
   end "=";

   -------------
   --  Image  --
   -------------

   function Image (Item : in Composite) return String is
      use Ada.Strings.Fixed;
      use Ada.Strings.Maps.Constants;
      use ESL.Parsing_Utilities;

   begin
      return
        Underscore_To_Dash (Translate (Source  => Item.Base'Img,
                                       Mapping => Lower_Case_Map)) & "/" &
        Underscore_To_Dash (Translate (Source  => Item.Sub'Img,
                                       Mapping => Lower_Case_Map));

   end Image;

   function Image (Item : in Content_Types) return String is
      use Ada.Strings.Fixed;
      use Ada.Strings.Maps.Constants;
      use ESL.Parsing_Utilities;
   begin
      return Underscore_To_Dash (Translate (Source  => Item'Img,
                                            Mapping => Lower_Case_Map));
   end Image;

   -------------
   --  Value  --
   -------------

   function Value (Item : in String) return Composite is
      use Ada.Strings.Fixed;
      use ESL.Parsing_Utilities;

      Context : constant String := Package_Name & ".Create";

      Seperator_Index : constant Natural := Index
        (Source  => Item,
         Pattern => Seperator);
      Key_Length     : constant Integer :=
        Seperator_Index - Seperator'Length - 1;
   begin
      return
        (Base =>
           Value (Dash_To_Underscore
             (Item (Item'First .. Item'First + Key_Length))),
         Sub  =>
           Value (Dash_To_Underscore
             (Item (Item'First + Seperator_Index .. Item'Last))));
   exception
      when Constraint_Error =>
         ESL.Trace.Debug (Message => "Could not convert " & Item,
                          Context => Context);
         raise;
   end Value;

   function Value (Item : in String) return Content_Types is
      use ESL.Parsing_Utilities;

      function Sanitize (Item : in String) return String;

      function Sanitize (Item : in String) return String is
      begin
         return Slash_To_Underscore (Dash_To_Underscore (Item));
      end Sanitize;

   begin
      return Content_Types'Value (Sanitize (Item));
   end Value;

   function Value (Item : in Composite) return Content_Types is
   begin
      raise Program_Error with "Not implemented";
      return Null_Value;
   end Value;

   function Value (Item : in Content_Types) return Composite is
   begin
      raise Program_Error with "Not implemented";
      return Null_Instance;
   end Value;

   -------------
   --  Value  --
   -------------

   function Value (Item : in String) return Types is
   begin
      return Types'Value (Item);
   end Value;

   -------------
   --  Value  --
   -------------

   function Value (Item : in String) return Subtypes is
   begin
      return Subtypes'Value (Item);
   end Value;

end ESL.Packet_Content_Type;
