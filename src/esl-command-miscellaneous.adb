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

with ESL.Command.Call_Management_Strings;

package body ESL.Command.Miscellaneous is
   function List_Users (Group   : in String := "";
                        Domain  : in String := "";
                        User    : in String := "";
                        Context : in String := "") return Instance
   is
      Obj : Instance;
   begin
      Obj.Set_Command (Call_Management_Strings.List_Users);
      Obj.Add_Component (Group);
      Obj.Add_Component (Domain);
      Obj.Add_Component (User);
      Obj.Add_Component (Context);

      return Obj;
   end List_Users;
end ESL.Command.Miscellaneous;
