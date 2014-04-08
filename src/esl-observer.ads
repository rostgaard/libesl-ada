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

with ESL.Packet;

private with Ada.Finalization;

package ESL.Observer is

   Package_Name : constant String := "ESL.Observer";

   type Observables is
     abstract tagged limited private;

   procedure Notify_Observers (Observing : in out Observables'Class;
                               Packet    : in     ESL.Packet.Instance);

   type Observers (Observing : access Observables'Class) is
     abstract tagged limited private;

   procedure Notify (Observer : access Observers;
                     Packet   : in     ESL.Packet.Instance) is abstract;

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

   type Observers (Observing : access Observables'Class) is
     abstract new Limited_Controlled with record
        Next : Observer_Access := null;
     end record;

   overriding
   procedure Initialize (Observer : in out Observers);

   overriding
   procedure Finalize (Observer : in out Observers);
end ESL.Observer;
