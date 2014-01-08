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

package ESL.Command.Miscellaneous is

   type Instance is new Command.Instance with null record;

   procedure Bg_System (Command : in String) is null;
   --  Execute a system command in the background.
   --  Usage: Bg_System <Command>

   procedure Echo (Message : in String) is null;
   --  Echo input back to the Console.

   procedure File_Exists (File : in String) is null;
   --  Tests whether filename exists. Returns string "true" or "false".

   procedure Find_User_XML (Key    : in String;
                            User   : in String;
                            Domain : in String) is null;
   --  Checks to see if a user exists; Matches user tags found in the
   --  directory, similar to user_exists, but returns an XML representation
   --  of the user as defined in the directory
   --  (like the one shown in user_exists).
   --  Usage: find_user_xml <key> <user> <domain>
   --  Where key references a key specified in a directory's user tag,
   --  user represents the value of the key, and the domain is the domain the
   --  user is assigned to.

   function List_Users (Group   : in String := "";
                        Domain  : in String := "";
                        User    : in String := "";
                        Context : in String := "") return Instance;
   --  Lists Users Configured in Directory
   --  Usage:
   --  list_users [group <group>] [domain <domain>]
   --             [user <user>] [context <context>]

   --  TODO Outline and imlement the rest.
end ESL.Command.Miscellaneous;
