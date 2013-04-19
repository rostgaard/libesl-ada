with ESL.Trace;

package body ESL.Packet is
   use ESL.Packet_Keys;

   procedure Add_Header (Obj   :     out Instance;
                         Field : in      Packet_Field.Instance) is
      Context : constant String := Package_Name & ".Add_Header";
   begin
      if Field.Key = Content_Type then
         Obj.Content_Type := Packet_Content_Type.Create (Field.Value);
      elsif Field.Key = Content_Length then
         Obj.Content_Length :=  Natural'Value (Field.Value);
      end if;
   end Add_Header;

   function Content_Length (Obj : in Instance) return Natural is
   begin
      return Obj.Content_Length;
   end Content_Length;

   function Equivalent_Keys (Left  : in Packet_Keys.Event_Keys;
                             Right : in Packet_Keys.Event_Keys) return Boolean
   is
   begin
      return Left = Right;
   end Equivalent_Keys;

   function Has_Header (Obj : in Instance;
                        Key : in Packet_Keys.Event_Keys) return Boolean is
      use Packet_Content_Type;
   begin
      if
        Key = Content_Type and
        Obj.Content_Type /= Packet_Content_Type.Null_Instance
      then
         return True;
      elsif Key = Content_Length then
         return Obj.Content_Length > 0;
      end if;

      return False;
   end Has_Header;

   function Hash_Header (Item : in Packet_Keys.Event_Keys) return
     Ada.Containers.Hash_Type is
   begin
      return Packet_Keys.Event_Keys'Pos (Item);
   end Hash_Header;

   function Image (Obj : in Instance) return String is
   begin
      return "Content_Type:" & Obj.Content_Type.Image &
        ", Content_Length:" & Obj.Content_Length'Img;
   end Image;
end ESL.Packet;
