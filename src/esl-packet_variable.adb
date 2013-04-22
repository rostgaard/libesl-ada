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

package body ESL.Packet_Variable is

   function Create (Name          : in String;
                    Initial_Value : in String := "") return Instance is
   begin
      return (Name  => To_Unbounded_String (Name),
              Value => To_Unbounded_String (Initial_Value));
   end Create;

   function Name (Obj : in Instance) return String is
   begin
      return To_String (Obj.Name);
   end Name;

   function Value (Obj : in Instance) return String is
   begin
      return To_String (Obj.Value);
   end Value;

end ESL.Packet_Variable;
