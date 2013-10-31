
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

--  This test case tests if various commands gets serialized correctly.

with Ada.Assertions;
with Ada.Command_Line;
with Ada.Text_IO;

with ESL.Trace;
with ESL.Command.Core;
with ESL.Command.Call_Management;

procedure ESL.Command.Test is
   use Ada.Assertions;
   use Ada.Text_IO;
   use ESL.Trace;

   procedure Originate_Test;
   --  Tests a basic orgination request.

   procedure Run_Test (Name : in String;
                       Test : not null access procedure);
   --  Runs the test. Should be moved to a "Test_Utilites package".

      procedure Show_Calls_Test;
   --  Tests a basic show calls command.

   procedure Show_Calls_Test_As_XML;
   --  Tests a basic show calls command appending "as xml".

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
         raise Assertion_Error with
         String (Command.Serialize) & " /= " & Expected;
      end if;
   end Originate_Test;

   ----------------
   --  Run_Test  --
   ----------------

   procedure Run_Test (Name : in String;
                       Test : not null access procedure) is
   begin
      Put (Name & " - ");
      Test.all;
      Put_Line ("success");
   exception
      when others =>
         Put_Line ("failure");
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
         raise;
   end Run_Test;

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

begin

   ESL.Trace.Unmute (Every);

   Run_Test (Name => "Creating show call command",
             Test => Show_Calls_Test'Access);

   Run_Test (Name => "Creating show call command (as xml)",
             Test => Show_Calls_Test_As_XML'Access);

   Run_Test (Name => "Creating originate command",
             Test => Originate_Test'Access);

end ESL.Command.Test;
