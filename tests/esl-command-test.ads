
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

with Ahven.Framework;

package ESL.Command.Test is

   Package_Name : constant String := "ESL.Command.Test";

   type Instance is new Ahven.Framework.Test_Case with null record;
   overriding
   procedure Initialize (T : in out Instance);
   overriding
   procedure Set_Up (T : in out Instance);

private
   procedure Originate_Test;
   --  Tests a basic orgination request.

   procedure Show_Calls_Test;
   --  Tests a basic show calls command.

   procedure Show_Calls_Test_As_XML;
   --  Tests a basic show calls command appending "as xml".

end ESL.Command.Test;
