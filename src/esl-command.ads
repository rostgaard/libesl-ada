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

private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Strings.Unbounded;

package ESL.Command is

   Package_Name : constant String := "ESL.Command";

   type Instance is abstract tagged private;

   type Data_Formats is (Unspecified, JSON, XML, Delim);
   --  Different data view formats for clients. Specifies which
   --  format the reply should be in.

   function Serialize (Obj : in Instance)
                       return Serialized_Command;

   function Image (Command : Serialized_Command) return String;

   procedure Set_Format (Obj       :    out Instance;
                         Format    : in     Data_Formats;
                         Delimiter : in     String := "");

   function "=" (Left, Right : in Serialized_Command) return Boolean;

private
   use Ada.Strings.Unbounded;

   Command_Seperator : constant String := " ";

   procedure Set_Command (Obj     :    out Instance;
                          Command : in     String);

   package Command_Component_Storage is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Unbounded_String);

   type Instance is abstract tagged
      record
         Command            : Unbounded_String;
         Command_Components : Command_Component_Storage.List;
         Show_As            : Data_Formats := Unspecified;
         Delimiter          : Unbounded_String := Null_Unbounded_String;
      end record;

   procedure Add_Component (Obj       :    out Instance;
                            Component : in     String);

end ESL.Command;
