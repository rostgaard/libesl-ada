with ESL.Packet_Keys;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
  
package ESL.Packet_Field is
   use ESL.Packet_Keys;
   
   Seperator : constant String := ":";   
   
   type Instance is tagged private;
   
   Null_Field : constant Instance;
   
   function Create (Key   : in String; 
                    Value : in String) return Instance;
   
   function Create (Key   : in Event_Keys; 
                    Value : in String) return Instance;
   
   function Image (Item : in Instance) return String;
   
private
   type Instance is tagged
      record
         Key   : Event_Keys;
         Value : Unbounded_String;
      end record;

   Null_Field : constant Instance := (Key   => Event_Keys '(Unknown),
                                    Value => Null_Unbounded_String);
   
   
end ESL.Packet_Field;
