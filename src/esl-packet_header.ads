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

with Ada.Containers.Hashed_Maps;
--  with Ada.Finalization;

with ESL.Header_Field;
with ESL.Packet_Keys;
with ESL.Packet_Content_Type;

package ESL.Packet_Header is
   use ESL.Packet_Content_Type;

   Seperator : constant String := ":";

   Package_Name : constant String := "ESL.Packet_Header";

   type Instance is tagged private;

   function Image (Item : in Instance) return String;

   procedure Add_Header (Obj   :     out Instance;
                         Field : in      Header_Field.Instance);

   function Content_Type (Obj : in Instance) return Content_Types;

   function Content_Length (Obj : in Instance) return Natural;

   function Empty (Obj : in Instance) return Boolean;

   Empty_Header : constant Instance;

private
   use Header_Field;

   function Equivalent_Keys (Left  : in Packet_Keys.Header_Keys;
                             Right : in Packet_Keys.Header_Keys)
                             return Boolean;

   function Hash_Header (Item : in Packet_Keys.Header_Keys) return
     Ada.Containers.Hash_Type;

   package Header_Storage is new Ada.Containers.Hashed_Maps
     (Key_Type        => Packet_Keys.Header_Keys,
      Element_Type    => Header_Field.Instance,
      Hash            => Hash_Header,
      Equivalent_Keys => Equivalent_Keys);

   type Instance is tagged
      record
         Content_Type   : Content_Types;
         Content_Length : Natural := 0;
         Fields         : Header_Storage.Map;
      end record;

   Empty_Header : constant Instance :=
     (Fields         => Header_Storage.Empty_Map,
      Content_Length => 0,
      Content_Type   => Null_Value);

end ESL.Packet_Header;
