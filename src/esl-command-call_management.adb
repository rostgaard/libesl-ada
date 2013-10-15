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

with Ada.Strings;
with Ada.Strings.Fixed;

with ESL.Command.Call_Management_Strings;

package body ESL.Command.Call_Management is
   use ESL.Command;

   procedure Add_Option (Obj    : in out Instance;
                         Option : in     ESL.Command.Option.Instance) is
   begin
      Obj.Options.Add (Option);
   end Add_Option;

   function Hangup (UUID : in String) return Instance is
      Obj : Instance;
   begin
      Obj.Set_Command (Call_Management_Strings.UUID_Kill);
      Obj.Add_Component (UUID);

      return Obj;
   end Hangup;

   function Originate (Call_URL         : in String;
                        --  URL you are calling.
                        Extension        : in String;
                        --  Destination number to enter dialplan with
                        Dialplan         : in String := "xml";
                        --  Which dialplan to perform the lookup in.
                        --  Defaults to 'xml'.
                        Context          : in String := "default";
                        --  Defaults to 'default'.
                        Caller_ID_Name   : in String := "";
                        --  CallerID name.
                        Caller_ID_Number : in String := "";
                        --  CallerID number.
                        Timeout          : in Duration := Duration'First
                        --  Timeout in seconds.
                      )
                       return Instance is
      Obj : Instance;
   begin
      Obj.Set_Command (Call_Management_Strings.Originate);
      Obj.Add_Component (Call_URL);
      Obj.Add_Component (Extension);
      Obj.Add_Component (Dialplan);
      Obj.Add_Component (Context);

      if Caller_ID_Name /= "" then
         Obj.Add_Component (Caller_ID_Name);
      end if;

      if Caller_ID_Number /= "" then
         Obj.Add_Component (Caller_ID_Number);
      end if;

      if Timeout /= Duration'First then

         Obj.Add_Component (Ada.Strings.Fixed.Trim
                            (Source => Timeout'Img,
                             Side   => Ada.Strings.Both));
      end if;

      return Obj;
   end Originate;

end ESL.Command.Call_Management;
