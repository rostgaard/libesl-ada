
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

package ESL.Job.List.Test is

   Package_Name : constant String := "ESL.Job.List.Test";

   type Instance is new Ahven.Framework.Test_Case with null record;
   overriding
   procedure Initialize (T : in out Instance);
   overriding
   procedure Set_Up (T : in out Instance);

private
   procedure Resubscribe_Test;
   --  Expected behaviour is that we do not want to be able to resubscribe
   --  more than one task to one job. Should raise an exception.

   procedure Early_Pop;
   --  Trying to pop a job from the queue is a contradition. The status can
   --  be polled with Reply_Ready prior to calling Pop.

   procedure Timeout_Test_No_Reply;
   --  When no reply is received within the given duration, a timeout
   --  exception is expected.

   procedure Timeout_Test_Reply;
   --  When a reply is filled into the shared buffer, no timeout is
   --  expected.

end ESL.Job.List.Test;
