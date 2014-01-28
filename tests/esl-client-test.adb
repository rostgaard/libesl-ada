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

with Ada.Strings.Unbounded;

with ESL;
with ESL.Client.Tasking;
with ESL.Trace;

package body ESL.Client.Test is
   use Ada.Strings.Unbounded;
   use ESL;
   use ESL.Trace;
   use ESL.Client.Tasking;

   Hostname : Unbounded_String := Null_Unbounded_String;
   Password : Unbounded_String := Null_Unbounded_String;
   Port     : Natural          := 0;
   Client   : ESL.Client.Tasking.Reference;


   procedure Tear_Down (T : in out Instance) is
      pragma Unreferenced (T);
   begin
      Client.Shutdown;
   end Tear_Down;

   -------------------------
   --  Authenticate_Fail  --
   -------------------------

   procedure Authenticate_Fail is
   begin
      Client.Connect (To_String (Hostname), Port);

      Client.Authenticate (Password => ASCII.EOT & ASCII.NUL);
   exception
      when Authentication_Failure =>
         null;
   end Authenticate_Fail;

   ----------------------------
   --  Authenticate_Success  --
   ----------------------------

   procedure Authenticate_Success is
   begin
      Client.Connect (To_String (Hostname), Port);

      Client.Authenticate (Password => To_String (Password));
      Client.Disconnect;
   end Authenticate_Success;

   ------------------
   --  Initialize  --
   ------------------

   procedure Initialize (T : in out Instance) is
   begin
      T.Set_Up;
      Set_Name (T, Package_Name);

      Ahven.Framework.Add_Test_Routine
        (T, Authenticate_Fail'Access, "Authentication_Fail");
      Ahven.Framework.Add_Test_Routine
        (T, Authenticate_Success'Access, "Authentication_Success");

      T.Tear_Down;
   end Initialize;

   --------------
   --  Set_Up  --
   --------------

   procedure Set_Up (T : in out Instance) is
      pragma Unreferenced (T);
   begin
      Port     := 8021;
      Hostname := To_Unbounded_String ("192.168.1.77");
      Password := To_Unbounded_String ("ClueCon");

      Client := new ESL.Client.Tasking.Instance (ESL.Client.Ignore_Event,
                                                 ESL.Client.Ignore_Event);
   end Set_Up;

end ESL.Client.Test;
