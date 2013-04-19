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
      if Item /= Empty_Line then
         return Item.Key'Img & Seperator & To_String (Item.Value);
      else
         return "";
      end if;
   end Image;

   function Key (Obj : in Instance) return ESL.Packet_Keys.Event_Keys is
   begin
      return Obj.Key;
   end Key;

   function Value (Obj : in Instance) return String is
   begin
      return To_String (Obj.Value);
   end Value;

end ESL.Packet_Field;
