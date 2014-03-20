private with Ada.Containers.Vectors;

package ESL.Packet.Buffer is

   use ESL.Packet;

   type Packet_Container is private;

   protected type Synchronized_Buffer is
      entry Pop (Packet : out ESL.Packet.Instance);
      procedure Push (Packet : out ESL.Packet.Instance);
   private
      Packet_Stack : Packet_Container;
   end Synchronized_Buffer;

private

   package Packet_Storage is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => ESL.Packet.Instance);
   type Packet_Container is new Packet_Storage.Vector with null record;

end ESL.Packet.Buffer;
