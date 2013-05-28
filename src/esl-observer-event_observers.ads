with ESL.Observer;
with ESL.Packet_Keys;

package ESL.Observer.Event_Observers is

   type Instance is abstract new ESL.Observer.Observers with private;
private

   type Instance is abstract
     new ESL.Observer.Observers with
      record
         Event : ESL.Packet_Keys.Event_Keys;
      end record;

end ESL.Observer.Event_Observers;
