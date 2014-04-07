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

package body ESL.Command.Option_List is

   procedure Add (Obj    : in out Instance;
                  Option : in     ESL.Command.Option.Instance) is
   begin
      Obj.Options.Append (New_Item => Option);
   end Add;

   function Serialize (Obj : in Instance)
                       return Serialized_Command is
      use Option_Storage;
      Buffer : Unbounded_String;
   begin

      if Obj.Options.Is_Empty then
         return "";
      end if;

      declare
         C : Cursor := Obj.Options.First;
      begin
         while C /= No_Element loop
            Append (Buffer,
                    To_Unbounded_String (String (Element (C).Serialize)));

            if C = Obj.Options.Last then
               exit;
            else
               Append (Buffer, Option_Separator);
            end if;

            Next (C);
         end loop;
      end;

      return Serialized_Command ("{" & To_String (Buffer) & "}");
   end Serialize;

end ESL.Command.Option_List;
