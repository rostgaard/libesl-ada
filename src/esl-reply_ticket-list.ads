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

private with Ada.Containers.Ordered_Maps;

package ESL.Reply_Ticket.List is

   Package_Name : constant String := "ESL.Reply_Ticket.List";

   type Instance is tagged limited private;

   procedure Add (List   : in out Instance;
                  Object : in     ESL.Reply_Ticket.Instance);

   procedure Remove (List   : in out Instance;
                     Object : in     ESL.Reply_Ticket.Instance);

   function Contains (List   : in Instance;
                      Object : in ESL.Reply_Ticket.Instance) return Boolean;

private
   package Ticket_Storage is new Ada.Containers.Ordered_Maps
     (Key_Type     => ESL.Reply_Ticket.Instance,
      Element_Type => Natural);

   protected type Synchronized_Storage is
      procedure Add (Item : in ESL.Reply_Ticket.Instance);
      function Contains (Object : in ESL.Reply_Ticket.Instance) return Boolean;
      procedure Remove (Item : in ESL.Reply_Ticket.Instance);
   private
      Protected_List : Ticket_Storage.Map;
   end Synchronized_Storage;

   type Instance is tagged limited
      record
         Storage : Synchronized_Storage;
      end record;

end ESL.Reply_Ticket.List;
