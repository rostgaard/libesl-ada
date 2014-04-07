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

with ESL.Case_Insensitive_Equal,
     ESL.Trace;

package body ESL.Command is

   -----------
   --  "="  --
   -----------

   function "=" (Left, Right : in Serialized_Command) return Boolean is
   begin
      return Case_Insensitive_Equal (Left  => String (Left),
                                     Right => String (Right));
   end "=";

   ---------------------
   --  Add_Component  --
   ---------------------

   procedure Add_Component (Obj       :    out Instance;
                            Component : in     String) is
   begin
      Obj.Command_Components.Append
        (New_Item => To_Unbounded_String (Component));
   end Add_Component;

   function Image (Command : Serialized_Command) return String is
   begin
      return String (Command);
   end Image;

   -----------------
   --  Serialize  --
   -----------------

   function Serialize (Obj : in Instance)
                       return Serialized_Command is
      use Command_Component_Storage;
      Buffer : Unbounded_String := Obj.Command & " ";
      C      : Cursor := Obj.Command_Components.First;
   begin
      while C /= No_Element loop
         Append (Buffer, Element (C));

         if C = Obj.Command_Components.Last then
            exit;
         else
            Append (Buffer, " ");
         end if;

         Next (C);
      end loop;

      case Obj.Show_As is
         when Unspecified =>
            null;
         when XML =>
            Append (Buffer, " as " & Obj.Show_As'Img);
         when JSON =>
            Append (Buffer, " as " & Obj.Show_As'Img);
         when Delim =>
            Append (Buffer, " as " & Obj.Show_As'Img & " ");
            Append (Buffer, Obj.Delimiter);
      end case;

      return Serialized_Command (To_String (Buffer));
   end Serialize;

   -------------------
   --  Set_Command  --
   -------------------

   procedure Set_Command (Obj     :    out Instance;
                          Command : in     String) is
   begin
      Obj.Command := To_Unbounded_String (Command);
   end Set_Command;

   ------------------
   --  Set_Format  --
   ------------------

   procedure Set_Format (Obj       :    out Instance;
                         Format    : in     Data_Formats;
                         Delimiter : in     String := "") is
      Context : constant String := Package_Name & ".Set_Format";
   begin
      Obj.Show_As := Format;

      if Format = Delim then
         Obj.Delimiter := To_Unbounded_String (Delimiter);
      elsif Format /= Delim and then Delimiter /= "" then
         ESL.Trace.Information
           (Message => "Ignoring delimiter on non-delimiter-requirering " &
                       "data format.",
           Context => Context);
      end if;
   end Set_Format;
end ESL.Command;
