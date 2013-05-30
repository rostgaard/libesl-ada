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

package body ESL.Packet_Header is
   use ESL.Trace;

   ------------------
   --  Add_Header  --
   ------------------

   procedure Add_Header (Obj   :     out Instance;
                         Field : in      Packet_Field.Instance) is
      Context : constant String := Package_Name & ".Add_Header";
   begin

      if Field = Empty_Line then
         ESL.Trace.Debug (Context => Context,
                          Message => "Skipping empty line");
         return;
      end if;

      ESL.Trace.Debug (Context => Context,
                       Message => "Adding " & Field.Image);

      Obj.Fields.Insert (Key      => Field.Key,
                          New_Item => Field);
   exception
      when others =>
         ESL.Trace.Error (Context => Context,
                          Message => "Error Adding " & Field.Image);
         raise;
   end Add_Header;

   function Empty (Obj : in Instance) return Boolean is
   begin
      return Obj.Fields.Is_Empty;
   end Empty;

   -----------------------
   --  Equivalent_Keys  --
   -----------------------

   function Equivalent_Keys (Left  : in Packet_Keys.Header_Keys;
                             Right : in Packet_Keys.Header_Keys) return Boolean
   is
      use type Packet_Keys.Header_Keys;
   begin
      return Left = Right;
   end Equivalent_Keys;

   -------------------
   --  Hash_Header  --
   -------------------

   function Hash_Header (Item : in Packet_Keys.Header_Keys) return
     Ada.Containers.Hash_Type is
   begin
      return Packet_Keys.Header_Keys'Pos (Item);
   end Hash_Header;
end ESL.Packet_Header;
