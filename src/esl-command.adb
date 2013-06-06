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

package body ESL.Command is

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
   begin
      for C in Obj.Command_Components.Iterate loop
         Append (Buffer, Element (C));

         if C /= Obj.Command_Components.Last then
            Append (Buffer, " ");
         end if;
      end loop;

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

end ESL.Command;
