package body ESL.Packet.Buffer is

   protected body Synchronized_Buffer is
      entry Pop (Packet : out ESL.Packet.Instance)
        when not Packet_Stack.Is_Empty
      is
      begin
         Packet := Packet_Stack.First_Element;
         Packet_Stack.Delete_First;
      end Pop;

      procedure Push (Packet : out ESL.Packet.Instance) is
      begin
         Packet_Stack.Append (New_Item => Packet);
      end Push;

   end Synchronized_Buffer;
end ESL.Packet.Buffer;
