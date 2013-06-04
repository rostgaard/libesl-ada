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

package body ESL.Command.Call_Management is

   function Originate (Call_URL         : in String;
                        --  URL you are calling.
                        Extension        : in String;
                        --  Destination number to enter dialplan with
                        Dialplan         : in String := "";
                        --  Which dialplan to perform the lookup in.
                        --  Defaults to 'xml'.
                        Context          : in String := "";
                        --  Defaults to 'default'.
                        Caller_ID_Name   : in String := "";
                        --  CallerID name.
                        Caller_ID_Number : in String := "";
                        --  CallerID number.
                        Timeout          : in Duration;
                        --  Timeout in seconds.
                        Options : Originate_Parameters)
                       return Instance is
      Obj : Instance;
   begin
      Obj.Add_Component (Call_URL);

      return Obj;
   end Originate;

   procedure Add_Option (Obj    : in out Instance;
                         Option : in     ESL.Command.Option_List.Options) is
   begin
      Obj.Options.Add (Option);
   end Add_Option;

end ESL.Command.Call_Management;
