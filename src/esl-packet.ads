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
                                   Raw_Data : in String) is null;

   function Image (Obj : in Instance) return String;

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
   end record;

end ESL.Packet;
