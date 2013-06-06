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

package body ESL.Command.Option is

   function "=" (Left, Right : in Instance) return Boolean is
   begin
      return Left.Key = Right.Key;
   end "=";

   function Create (Key   : in String;
                    Value : in String) return Instance is
   begin
      return (Key   => To_Unbounded_String (Key),
              Value => To_Unbounded_String (Value));
   end Create;

   function Serialize (Obj : in Instance)
                       return Serialized_Command is
   begin
      return Serialized_Command
        (To_String (Obj.Key) &
           Key_Value_Separator & To_String (Obj.Value));
   end Serialize;

end ESL.Command.Option;
