package ESL.Packet_Content_Types is
   type Response_Type is (Auth, Text, Command);

   type Content_Type is (Request, Event_Plain, Reply);
end ESL.Packet_Content_Types;
