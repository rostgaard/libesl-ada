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

with ESL.Trace;

package body ESL.Reply_Ticket.List is

   procedure Add (List   : in out Instance;
                  Object : in     ESL.Reply_Ticket.Instance) is
   begin
      List.Storage.Add (Object);
   end Add;

   function Contains (List   : in Instance;
                      Object : in ESL.Reply_Ticket.Instance) return Boolean is
   begin
      return List.Storage.Contains (Object);
   end Contains;

   procedure Remove (List   : in out Instance;
                     Object : in     ESL.Reply_Ticket.Instance) is
   begin
      List.Storage.Remove (Object);
   end Remove;

   protected body Synchronized_Storage is

      procedure Add (Item : in ESL.Reply_Ticket.Instance) is
         use Ticket_Storage;

         procedure Update (Key     : in ESL.Reply_Ticket.Instance;
                           Element : in out Natural);

         procedure Update (Key     : in ESL.Reply_Ticket.Instance;
                           Element : in out Natural) is
            pragma Unreferenced (Key);
         begin
            Element := Element + 1;
         end Update;

         C : constant Cursor := Protected_List.Find (Item);
      begin
         if C = No_Element then
            Protected_List.Insert (Item, 0);
         else
            Protected_List.Update_Element (Position => C,
                                           Process  => Update'Access);
         end if;
      end Add;

      function Contains (Object : in ESL.Reply_Ticket.Instance)
                         return Boolean
      is
      begin
         return Protected_List.Contains (Object);
      end Contains;

      procedure Remove (Item : in ESL.Reply_Ticket.Instance) is
         use Ticket_Storage;

         Context : constant String := Package_Name &
           ".Synchronized_Storage.Remove";

         procedure Update (Key     : in ESL.Reply_Ticket.Instance;
                           Element : in out Natural);

         procedure Update (Key     : in ESL.Reply_Ticket.Instance;
                           Element : in out Natural) is
            pragma Unreferenced (Key);
         begin
            Element := Element - 1;
         end Update;

         C : Cursor := Protected_List.Find (Item);
      begin
         if C = No_Element then
            ESL.Trace.Error
              (Message => "No element to remove from ticket list!",
              Context => Context);
         elsif Element (C) < 2 then -- Removing the last element.
            Protected_List.Delete (Position => C);
         else
            Protected_List.Update_Element (Position => C,
                                           Process  => Update'Access);
         end if;
      end Remove;

   end Synchronized_Storage;
end ESL.Reply_Ticket.List;
