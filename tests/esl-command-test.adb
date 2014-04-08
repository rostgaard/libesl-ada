
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

with Ada.Assertions;

with ESL.Trace;
with ESL.Command.Core;
with ESL.Command.Call_Management;

package body ESL.Command.Test is
   use Ada.Assertions;

   overriding
   procedure Initialize (T : in out Instance) is
   begin
      Set_Name (T, Package_Name);

      Ahven.Framework.Add_Test_Routine
        (T, Originate_Test'Access, "Originate_Test");
      Ahven.Framework.Add_Test_Routine
        (T, Show_Calls_Test'Access, "Show_Calls_Test");
      Ahven.Framework.Add_Test_Routine
        (T, Show_Calls_Test_As_XML'Access, "Show_Calls_Test_As_XML");
   end Initialize;

   ----------------------
   --  Originate_Test  --
   ----------------------

   procedure Originate_Test is
      Command : constant ESL.Command.Call_Management.Instance :=
        ESL.Command.Call_Management.Originate
          (Call_URL         => "user/1001",
           Extension        => "5900");
      Expected : constant String := "originate user/1001 5900 xml default";
   begin
      if Command.Serialize /= Serialized_Command (Expected) then
         Ahven.Fail (String (Command.Serialize) & " /= " & Expected);
      end if;
   end Originate_Test;

   overriding
   procedure Set_Up (T : in out Instance) is
      pragma Unreferenced (T);
   begin
      ESL.Trace.Unmute (ESL.Trace.Every);
   end Set_Up;

   -----------------------
   --  Show_Calls_Test  --
   -----------------------

   procedure Show_Calls_Test is
      Report   : constant String := "calls";
      Command  : constant ESL.Command.Core.Instance :=
        ESL.Command.Core.Show (Report => Report);
      Expected : constant String := "show calls";
   begin
      if Command.Serialize /= Serialized_Command (Expected) then
         raise Assertion_Error;
      end if;
   end Show_Calls_Test;

   ------------------------------
   --  Show_Calls_Test_As_XML  --
   ------------------------------

   procedure Show_Calls_Test_As_XML is
      Report   : constant String := "calls";
      Command  : ESL.Command.Core.Instance :=
        ESL.Command.Core.Show (Report => Report);
      Expected : constant String := "show calls as xml";
   begin
      Command.Set_Format (Format => XML);
      if Command.Serialize /= Serialized_Command (Expected) then
         raise Assertion_Error;
      end if;
   end Show_Calls_Test_As_XML;

end ESL.Command.Test;
