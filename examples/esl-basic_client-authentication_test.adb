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
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions;

with ESL;
with ESL.Trace;

procedure ESL.Basic_Client.Authentication_Test is
   use Ada.Strings.Unbounded;
   use ESL;
   use ESL.Trace;
   use ESL.Reply;

   procedure Usage;

   procedure Usage is
   begin
      Put_Line ("Usage:" & Command_Name & " hostname port password");
      Set_Exit_Status (Failure);
   end Usage;

   Client   : ESL.Basic_Client.Instance;
   Hostname : Unbounded_String;
   Port     : Natural := 0;

   procedure Connection_Test is
   begin
      if Argument_Count < 3 then
         Usage;
         Set_Exit_Status (Failure);
         return;
      end if;

      Client.Connect( To_String (Hostname), Natural'Value (Argument (2)));

      Ada.Text_IO.Put ("Expecting Authentication_Failure Exception .. ");
      declare
      begin
         Client.Authenticate
           (Password => ASCII.EOT & ASCII.NUL);
         Put_Line ("Failed!");
         Set_Exit_Status (Failure);
      exception
         when Authentication_Failure =>
            Ada.Text_IO.Put_Line ("OK.");
      end;

      --  Wait for the client to get back on its feet, and then reconnect.
      Connect (Client, Argument (1), Natural'Value (Argument (2)));

      declare
      begin
         Ada.Text_IO.Put ("Expecting authentication to succeed .. ");
         Client.Authenticate
           (Password => Argument (3));
         Put_Line ("OK.");
      exception
         when Authentication_Failure =>
            Put_Line ("Failed.");
            Set_Exit_Status (Failure);
      end;

      Client.Disconnect;
   exception
      when E : others =>
         Ada.Text_IO.Put
           ("Oh noes! .. " & Ada.Exceptions.Exception_Information (E));
         Client.Disconnect;

   end Connection_Test;

   task Connect_To_Invalid is
      entry Start;
   end Connect_To_Invalid;

   task body Connect_To_Invalid is
   begin
      accept Start;
      Client.Connect ("example.com", 8021);
   end Connect_To_Invalid;

begin
   Set_Exit_Status (Ada.Command_Line.Success);
   ESL.Trace.UnMute (ESL.Trace.Every);

   Client.Disconnect;

   Hostname := To_Unbounded_String (Argument (1));

   for I in 1 .. 2 loop
      Connection_Test;
   end loop;

   Connect_To_Invalid.Start;
   delay 1.0;
   Client.Disconnect;


end ESL.Basic_Client.Authentication_Test;
