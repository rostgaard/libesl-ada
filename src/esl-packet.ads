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

with ESL.Packet_Content_Type;
with ESL.Packet_Field;
with ESL.Packet_Keys;

package ESL.Packet is
   use ESL;
   use Packet_Field;

   Package_Name : constant String := "ESL.Packet";

   type Instance is tagged private;

   procedure Add_Header (Obj   :     out Instance;
                         Field : in      Packet_Field.Instance);

   function Has_Header (Obj : in Instance;
                        Key : in Packet_Keys.Event_Keys) return Boolean;

   function Content_Length (Obj : in Instance) return Natural;

   procedure Process_And_Add_Body (Obj      : in Instance;
                                   Raw_Data : in String);

   function Image (Obj : in Instance) return String;

   function Create return Instance;

private

   function Hash_Header (Item : in Packet_Keys.Event_Keys) return
     Ada.Containers.Hash_Type;

   function Equivalent_Keys (Left  : in Packet_Keys.Event_Keys;
                             Right : in Packet_Keys.Event_Keys) return Boolean;

   package Header_Storage is new Ada.Containers.Hashed_Maps
     (Key_Type        => Packet_Keys.Event_Keys,
      Element_Type    => Packet_Field.Instance,
      Hash            => Hash_Header,
      Equivalent_Keys => Equivalent_Keys);

   type Instance is tagged record
      Content_Type   : Packet_Content_Type.Instance :=
        Packet_Content_Type.Null_Instance;
      Content_Length : Natural := 0;

      Payload        : Header_Storage.Map;
      --  TODO      Variables      :
   end record;

end ESL.Packet;
