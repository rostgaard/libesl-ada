-------------------------------------------------------------------------------
--                                                                           --
--                     Copyright (C) 2012-, AdaHeads K/S                     --
--                                                                           --
--  This is free software;  you can redistribute it and/or modify it         --
--  under terms of the  GNU General Public License  as published by the      --
--  Free Software  Foundation;  either version 3,  or (at your  option) any  --
--  later version. This library is distributed in the hope that it will be   --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     --
--  You should have received a copy of the GNU General Public License and    --
--  a copy of the GCC Runtime Library Exception along with this program;     --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
--  <http://www.gnu.org/licenses/>.                                          --
--                                                                           --
-------------------------------------------------------------------------------

with ESL.Trace;

package body ESL.Observer is

   overriding
   procedure Finalize (Observer : in out Observers) is
   begin
      Unregister (Observer.Observing, Observer'Access);
   end Finalize;

   overriding
   procedure Initialize (Observer : in out Observers) is
   begin
      Register (Observer.Observing, Observer'Access);
   end Initialize;

   procedure Notify_Observers (Observing : in out Observables'Class;
                               Packet    : in     ESL.Packet.Instance) is
      Context : constant String := Package_Name & ".Notify_Observers";

      Observer : Observer_Access := Observing.Observer_List;
   begin
      if Observer = null then
         ESL.Trace.Debug (Message => "None subscribed to event " &
                            Packet.Event'Img & " - consider muting.",
                          Context => Context);
      end if;

      while Observer /= null loop
         Notify (Observer => Observer,
                 Packet   => Packet);
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
