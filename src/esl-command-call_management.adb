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

   ------------------
   --  Add_Option  --
   ------------------

   procedure Add_Option (Obj    : in out Instance;
                         Option : in     ESL.Command.Option.Instance) is
   begin
      Obj.Options.Add (Option);
   end Add_Option;

   -----------------
   --  Originate  --
   -----------------

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

   -------------------
   --  UUID_Bridge  --
   -------------------

   function UUID_Bridge (UUID       : in ESL.UUID.Instance;
                         UUID_Other : in ESL.UUID.Instance) return Instance is
      Obj : Instance;
   begin
      Obj.Set_Command (Call_Management_Strings.UUID_Bridge);
      Obj.Add_Component (UUID.Serialize);
      Obj.Add_Component (UUID_Other.Serialize);

      return Obj;
   end UUID_Bridge;

   -----------------
   --  UUID_Kill  --
   -----------------

   function UUID_Kill (UUID : in String;
                       Cause : in Hangup_Cause := -1) return Instance is
      Obj : Instance;
   begin
      Obj.Set_Command (Call_Management_Strings.UUID_Kill);
      Obj.Add_Component (UUID);

      if Cause >= 0 then
         Obj.Add_Component (Cause'Img);
      end if;

      return Obj;
   end UUID_Kill;

   -----------------
   --  UUID_Park  --
   -----------------

   function UUID_Park (UUID : in String) return Instance is
      Obj : Instance;
   begin
      Obj.Set_Command (Call_Management_Strings.UUID_Park);
      Obj.Add_Component (UUID);

      return Obj;
   end UUID_Park;

   ---------------------
   --  UUID_Transfer  --
   ---------------------

   function UUID_Transfer (UUID        : in ESL.UUID.Instance;
                           Destination : in String;
                           Mode        : in Transfer_Modes := Undefined;
                           Dialplan    : in String := "";
                           Context     : in String := "") return Instance is
   begin
      return Obj : Instance do
         Obj.Set_Command (Call_Management_Strings.UUID_Transfer);
         Obj.Add_Component (UUID.Serialize);

         if Mode /= Undefined then
            Obj.Add_Component ("-" & Mode'Img);
         end if;

         Obj.Add_Component (Destination);

         if Dialplan /= "" then
            Obj.Add_Component (Dialplan);
         elsif Context /= "" then
            Obj.Add_Component (Context);
         end if;

      end return;
   end UUID_Transfer;

end ESL.Command.Call_Management;
