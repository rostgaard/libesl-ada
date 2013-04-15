package ESL.Core_Command is
   type Instance is abstract tagged null record;

   procedure Acl (IP_Address : String;
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

end ESL.Core_Command;
