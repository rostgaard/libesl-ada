package body ESL.Packet_Field is
   
   function Create (Key   : in String; 
                    Value : in String) return Instance is
   begin
      return Create (Key   => Event_Keys'Value (Key),
                     Value => Value);
   end Create;
   
   function Create (Key   : in Event_Keys; 
                    Value : in String) return Instance is
   begin
      return  (Key   => Key,
               Value => To_Unbounded_String (Value));
   end Create;
   
   function Image (Item : in Instance) return String is
   begin
      return Item.Key'Img & Seperator & To_String (Item.Value);
   end Image;
   
   
end ESL.Packet_Field;
