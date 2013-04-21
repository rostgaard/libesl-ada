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

   function "=" (Left  : in Instance;
                 Right : in Instance) return Boolean is
   begin
      return
        (Left.Base = Right.Base) and
        (Left.Sub  = Right.Sub);
   end "=";

   --------------
   --  Create  --
   --------------

   function Create (Item : in String) return Instance is
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
   end Create;

   -------------
   --  Image  --
   -------------

   function Image (Item : in Instance) return String is
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
