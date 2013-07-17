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

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions;

with ESL.Client.Tasking;

with ESL;
with ESL.Trace;

procedure ESL.Client.Tasking.Start_Stop_Test is
   use ESL;
   use ESL.Trace;

   Client : ESL.Client.Tasking.Instance;

   procedure Usage;

   procedure Usage is
   begin
      Put_Line ("Usage:" & Command_Name & " hostname port");
      Set_Exit_Status (Failure);
   end Usage;

   task Tasking_Connect is
      entry Start;
      entry Abort_Task;
   end Tasking_Connect;

   task body Tasking_Connect is
      Started : Boolean := False;
   begin
      select
         accept Start do
            Started := True;
         end Start;
      or
         accept Abort_Task do
            Shutdown (Client);
         end Abort_Task;
      end select;

      if Started then
         Client.Connect (Argument (1), Natural'Value (Argument (2)));
      end if;

   end Tasking_Connect;

begin

   if Argument_Count < 2 then
      Usage;
      Tasking_Connect.Abort_Task;
      return;
   end if;

   Tasking_Connect.Start;
   delay 3.0;
   Shutdown (Client);
   Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Success);

   exception
      when E : others =>
         ESL.Trace.Error (Message => Ada.Exceptions.Exception_Information (E),
                          Context => "ESL.Client.Tasking.Test");
      Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Failure);

end ESL.Client.Tasking.Start_Stop_Test;
