with ESL.Packet;
with ESL.Client;

private with Ada.Finalization;

package ESL.Observer is
   type Observables is
     abstract tagged limited private;

   procedure Notify_Observers (Subject  : in out Observables;
                               Packet   : in     ESL.Packet.Instance;
                               Client   : in     ESL.Client.Reference);

   type Observers (Subject : access Observables'Class) is
     abstract tagged limited private;

   procedure Notify (Observer : access Observers;
                     Packet   : in     ESL.Packet.Instance;
                     Client   : in     ESL.Client.Reference) is abstract;

private
   use Ada.Finalization;

   type Observer_Access is access all Observers'Class;
   pragma Suppress (Accessibility_Check, On => Observer_Access);
   pragma Suppress (Access_Check, On => Observer_Access);

   type Observables is
     abstract tagged limited record
        Observer_List : Observer_Access;
     end record;

   procedure Register
     (Subject  : access Observables;
      Observer : access Observers'Class);

   procedure Unregister
     (Subject  : access Observables;
      Observer : access Observers'Class);

   type Observers (Subject : access Observables'Class) is
     abstract new Limited_Controlled with record
        Next : Observer_Access := null;
     end record;

   procedure Initialize (Observer : in out Observers);

   procedure Finalize (Observer : in out Observers);
end ESL.Observer;
