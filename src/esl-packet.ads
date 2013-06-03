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
private with Ada.Strings.Unbounded;
private with GNATCOLL.JSON;

with ESL.Packet_Field;
with ESL.Packet_Header;
with ESL.Header_Field;
with ESL.Packet_Keys;
with ESL.Packet_Content_Type;

package ESL.Packet is
   use ESL;
   use Packet_Field;
   use Packet_Content_Type;

   Package_Name : constant String := "ESL.Packet";

   type Instance is tagged private;

   procedure Push_Header (Obj   :    out Instance;
                          Field : in     Header_Field.Instance);

   function Payload (Obj : in Instance) return String;

   function Create return Instance;

   function Content_Length (Obj : in Instance) return Natural;
   function Content_Type (Obj : in Instance) return Content_Types;

   procedure Process_And_Add_Body (Obj      : in out Instance;
                                   Raw_Data : in     String);

   function Image (Obj : in Instance) return String;

   function Contains (Obj : in Instance;
                      Key : in Packet_Keys.Event_Keys) return Boolean;

   function Field (Obj : in Instance;
                   Key : in Packet_Keys.Event_Keys)
                   return Packet_Field.Instance;

   function Is_Event (Obj : in Instance) return Boolean;
   function Is_Response (Obj : in Instance) return Boolean;

   function Event (Obj : in Instance) return Packet_Keys.Inbound_Events;
private
   use Ada.Strings.Unbounded;
   use Ada.Strings;

   function Equivalent_Keys (Left  : in Packet_Keys.Event_Keys;
                             Right : in Packet_Keys.Event_Keys) return Boolean;

   function Equivalent_Keys (Left  : in Unbounded_String;
                             Right : in Unbounded_String) return Boolean;
   function Hash_Field (Item : in Packet_Keys.Event_Keys) return
     Ada.Containers.Hash_Type;

   package Payload_Storage is new Ada.Containers.Hashed_Maps
     (Key_Type        => Packet_Keys.Event_Keys,
      Element_Type    => Packet_Field.Instance,
      Hash            => Hash_Field,
      Equivalent_Keys => Equivalent_Keys);

   function Image (List : Payload_Storage.Map) return String;

   type Instance is tagged record
      Header         : Packet_Header.Instance   := Packet_Header.Empty_Header;
      Raw_Body       : Unbounded_String         := Null_Unbounded_String;
      JSON           : GNATCOLL.JSON.JSON_Value := GNATCOLL.JSON.Create;
      Payload        : Payload_Storage.Map      := Payload_Storage.Empty_Map;
   end record;

end ESL.Packet;
