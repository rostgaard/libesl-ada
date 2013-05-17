package body ESL.Observer is
--     function "=" (Left, Right : in Reference) return Boolean is
--     begin
--        return True;
--     end "=";

   procedure Finalize (Observer : in out Observers) is
   begin
      Unregister (Observer.Subject, Observer'Access);
   end Finalize;

   procedure Initialize (Observer : in out Observers) is
   begin
      Register (Observer.Subject, Observer'Access);
   end Initialize;

   procedure Notify_Observers (Observing : in out Observables;
                               Packet    : in     ESL.Packet.Instance;
                               Client    : in     ESL.Client.Reference) is
      Observer : Observer_Access := Observing.Observer_List;
   begin
      while Observer /= null loop
         Notify (Observer => Observer,
                 Packet   => Packet,
                 Client   => Client);
         Observer := Observer.Next;
      end loop;
   end Notify_Observers;

   procedure Register
     (Subject  : access Observables;
      Observer : access Observers'Class) is
   begin
      Observer.Next := Subject.Observer_List;
      Subject.Observer_List := Observer_Access (Observer);
   end Register;

   procedure Unregister
     (Subject  : access Observables;
      Observer : access Observers'Class) is

      Object  : constant Observer_Access := Observer_Access (Observer);

      Prev    : Observer_Access;
      Current : Observer_Access;
   begin
      if Subject.Observer_List = Object then
         Subject.Observer_List := Subject.Observer_List.Next;
      else
         Prev    := Subject.Observer_List;
         Current := Prev.Next;

         while Current /= Object loop
            Prev := Current;
            Current := Current.Next;
         end loop;

         Prev.Next := Current.Next;
      end if;
   end Unregister;

end ESL.Observer;
