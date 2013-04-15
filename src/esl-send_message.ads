package ESL.Send_Message is
   type Instance is abstract tagged null record;

   type Inbound_Instance is new Instance with null record;

   type Outbound_Instance is new Instance with null record;

end ESL.Send_Message;
