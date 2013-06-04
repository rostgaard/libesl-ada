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

with ESL.Command.Option;

package ESL.Command.Option_List is

   type Instance is tagged private;

   procedure Add (Obj    : in out Instance;
                  Option : in     ESL.Command.Option.Instance);

   function Serialize (Obj : in Instance)
                       return Serialized_Command;
private
   use ESL.Command.Option;

   package Option_Storage is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => ESL.Command.Option.Instance);

   type Instance is tagged
      record
         Command_Components : Command_Component_Storage.List;
      end record;

   procedure Add_Component (Obj       :    out Instance;
                            Component : in     String);

end ESL.Command.Option_List;
