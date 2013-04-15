package ESL.Event is

   type Event_Listener is interface;

   procedure Trigger (Obj   : in Event_Listener;
                      Event : in String) is abstract;

end ESL.Event;
