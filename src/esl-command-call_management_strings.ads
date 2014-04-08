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

package ESL.Command.Call_Management_Strings is
   --  Call Management strings.

   Originate     : constant String := "originate";
   UUID_Break    : constant String := "uuid_break";
   UUID_Bridge   : constant String := "uuid_bridge";
   UUID_Kill     : constant String := "uuid_kill";
   UUID_Park     : constant String := "uuid_park";
   UUID_Transfer : constant String := "uuid_transfer";

   --  Miscellaneous command strings.
   List_Users    : constant String := "list_users";
   Flag_All      : constant String := "all";

end ESL.Command.Call_Management_Strings;
