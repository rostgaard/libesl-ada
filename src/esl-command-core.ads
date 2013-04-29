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

package ESL.Command.Core is
   use ESL;

   type Instance is tagged private;

   function Serialize (Obj : in Instance)
                       return Serialized_Command;

   procedure ACL (IP_Address : String;
                  List       : String) is null;
   --  Compare an IP to an ACL list.
   --  Usage: acl <ip> <list_name>

   procedure Add_Alias (Alias   : String;
                        Command : String) is null;
   --  A means to save some keystrokes/bytes on commonly used commands.

   procedure Remove_Alias (Alias   : String;
                           Command : String) is null;
   --  Usage: alias add <alias> <command> | del [<alias>|*]

   procedure Purge_Alias is null;
   --  Usage: alias add <alias> <command> | del [<alias>|*]

   procedure Background_API (Commmand : Command.Instance) is null;

private
   type Instance is new Command.Instance with null record;
end ESL.Command.Core;
