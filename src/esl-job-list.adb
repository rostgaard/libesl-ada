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

with Ada.Calendar;
with Ada.Exceptions;
with Ada.Strings.Unbounded;

with ESL.Trace;
with ESL.Packet_Keys;

package body ESL.Job.List is
   use Ada.Exceptions;

   procedure Discard (List : in out Instance;
                      UUID : in ESL.UUID.Instance) is
   begin
      List.Storage.Discard (UUID => UUID);
   end Discard;

   function Image (List : in Instance) return String is
   begin
      return List.Storage.Image;
   end Image;

   overriding
   procedure Notify (Observer : access Job_Observer;
                     Packet   : in     ESL.Packet.Instance) is
   begin
      Observer.Job_List.Push (Packet => Packet);
   end Notify;

   procedure Pop (List : in out Instance;
                  UUID : in ESL.UUID.Instance;
                  Job  :    out ESL.Job.Instance) is
   begin
      List.Storage.Pop (UUID => UUID,
                        Job  => Job);
   end Pop;

   procedure Push (List   : in out Instance;
                   Packet : in ESL.Packet.Instance) is
   begin
      List.Storage.Push (Packet);
   end Push;

   function Reply_Ready (List : in Instance;
                         UUID : in ESL.UUID.Instance) return Boolean is
   begin
      return List.Storage.Reply_Ready (UUID => UUID);
   end Reply_Ready;

   procedure Subscribe (List : in out Instance;
                        UUID : in ESL.UUID.Instance) is
   begin
      List.Storage.Subscribe (UUID);
   end Subscribe;

   protected body Synchronized_Storage is

      procedure Discard (UUID : in ESL.UUID.Instance) is
         use Job_Storage;

         Context : constant String := Package_Name &
           ".Synchronized_Storage.Discard";

      begin
         if not Unclaimed.Contains (UUID) then
            ESL.Trace.Debug (Message => "Inserting UUID: " & UUID.Image,
                             Context => Context);
            Claimed.Insert
              (Key      => UUID,
               New_Item => ESL.Job.Create (UUID      => UUID,
                                           When_Done => Discard));
         else
            ESL.Trace.Debug (Message => "Deleting UUID:" & UUID.Image,
                             Context => Context);
            Unclaimed.Delete (Key => UUID);
         end if;
      end Discard;

      function Has_Reply (UUID : in ESL.UUID.Instance) return Boolean is
      begin
         if not Claimed.Contains (UUID) then
            return False;
         elsif Claimed.Element (UUID).Action /= Ready then
            return False;
         else
            return True;
         end if;
      end Has_Reply;

      function Image return String is
         use Ada.Strings.Unbounded;
         use Job_Storage;

         Buffer : Unbounded_String;

         C  : Cursor := Claimed.First;
      begin
         if C = No_Element then
            return "<empty list>";
         end if;

         while C /= No_Element loop
            Append (Buffer, Element (C).Image & " (claimed) " & ASCII.LF);
            C := Next (C);
         end loop;

         C := Unclaimed.First;

         while C /= No_Element loop
            Append (Buffer, Element (C).Image & " (unclaimed) " & ASCII.LF);
            C := Next (C);
         end loop;

         return To_String (Buffer);
      end Image;

      procedure Pop  (UUID : in     ESL.UUID.Instance;
                      Job  :    out ESL.Job.Instance) is
         use ESL.Packet;
      begin
         if not Has_Reply (UUID => UUID) then
            raise Not_Done with "No reply received yet.";
         else
            Job := Claimed.Element (Key => UUID);
            Claimed.Delete (Key => UUID);
         end if;
      end Pop;

      procedure Push (Packet : in ESL.Packet.Instance) is
         use Job_Storage;

         Context : constant String := Package_Name &
           ".Synchronized_Storage.Push";

         procedure Update (Key     : in     ESL.UUID.Instance;
                           Element : in out ESL.Job.Instance);

         procedure Update (Key     : in     ESL.UUID.Instance;
                           Element : in out ESL.Job.Instance) is
            pragma Unreferenced (Key);
         begin
            if Element.Action /= Ready then
               Element.Action := Ready;
               Element.Packet := Packet;
            else
               raise Constraint_Error with "Multiple packet received on job " &
                 Element.Image;
            end if;
         end Update;

         UUID : constant ESL.UUID.Instance :=
           ESL.UUID.Create
             (Item => Packet.Field
                  (Key => ESL.Packet_Keys.Job_UUID).Decoded_Value);
         Claimed_C   : constant Cursor := Claimed.Find (UUID);
         Unclaimed_C : constant Cursor := Unclaimed.Find (UUID);
      begin
         if Unclaimed_C = No_Element and Claimed_C = No_Element then
            declare
               Job : ESL.Job.Instance :=
                 ESL.Job.Create (UUID      => UUID,
                                 When_Done => Unknown);
            begin
               ESL.Trace.Debug (Message => "New UUID: " & UUID.Image,
                                Context => Context);

               Job.Set_Packet (Packet => Packet);
               Unclaimed.Insert (UUID, Job);
            end;
         elsif Claimed_C /= No_Element then
            ESL.Trace.Debug (Message => "UUID: " & UUID.Image &
                               " found in Claimed",
                             Context => Context);

            if Element (Claimed_C).Action /= Discard then
               Claimed.Update_Element (Position => Claimed_C,
                                       Process  => Update'Access);
            else
               Claimed.Delete (Key => UUID);
            end if;
         elsif Unclaimed_C /= No_Element then
            ESL.Trace.Debug (Message => "UUID: " & UUID.Image &
                               " found in Unclaimed",
                             Context => Context);
            if Element (Unclaimed_C).Action /= Discard then
               Unclaimed.Update_Element (Position => Unclaimed_C,
                                         Process  => Update'Access);
            else
               Unclaimed.Delete (Key => UUID);
            end if;

         end if;
      end Push;

      function Reply_Ready (UUID : in ESL.UUID.Instance) return Boolean is
      begin
         return Has_Reply (UUID => UUID);
      end Reply_Ready;

      procedure Subscribe (UUID : in ESL.UUID.Instance) is
         use Job_Storage;

         Context : constant String := Package_Name &
           ".Synchronized_Storage.Subscribe";

      begin
         if Unclaimed.Contains (UUID) then
            declare
               Job : ESL.Job.Instance := Unclaimed.Element (UUID);
            begin
               --  Move the element to the correct container.
               ESL.Trace.Debug
                 (Message => "Moving UUID "  & UUID.Image &
                    " from unclaimed to claimed",
                  Context => Context);
               Job.Action := Keep;
               Claimed.Insert (Key      => UUID,
                               New_Item => Job);

               Unclaimed.Delete (UUID);
            end;
         else
            ESL.Trace.Debug (Message => "Claiming UUID " & UUID.Image,
                             Context => Context);
            Claimed.Insert
              (Key      => UUID,
               New_Item => ESL.Job.Create (UUID      => UUID,
                                           When_Done => Keep));

         end if;
      exception
         when E : others =>
            ESL.Trace.Error (Message => Exception_Information (E),
                             Context => Context);
            raise;
      end Subscribe;

   end Synchronized_Storage;

   procedure Wait_For (List    : in out Instance;
                       UUID    : in     ESL.UUID.Instance;
                       Job     :    out ESL.Job.Instance;
                       Timeout : in     Duration := 2.0) is
      use type Ada.Calendar.Time;

      Context  : constant String := Package_Name & ".Wait_For";
      pragma Unreferenced (Context);

      Deadline : constant Ada.Calendar.Time := Ada.Calendar.Clock + Timeout;
   begin
      if List.Reply_Ready (UUID) then
         List.Pop (UUID => UUID,
                   Job  => Job);
         return;
      end if;

      loop
         exit when Ada.Calendar.Clock > Deadline;
         if List.Reply_Ready (UUID) then
            List.Pop (UUID => UUID,
                      Job  => Job);
            return;
         end if;

         delay 0.01;
      end loop;

      raise Timeout_Reached;

   end Wait_For;

end ESL.Job.List;
