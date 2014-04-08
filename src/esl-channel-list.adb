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

package body ESL.Channel.List is

   protected body Synchronized_Storage is

      procedure Change_State (Key       : in     ESL.Channel.Channel_Key;
                              New_State : in     ESL.Channel.States) is
         use Channel_Storage;

         Context : constant String := Package_Name &
           ".Synchronized_Storage.Change_State";

         procedure Process (Key : in     Channel.Channel_Key;
                            Ch  : in out Channel.Instance);

         procedure Process (Key : in     Channel.Channel_Key;
                            Ch  : in out Channel.Instance) is
            pragma Unreferenced (Key);
         begin
            Ch.State := New_State;
         end Process;

         C : constant Cursor := Storage.Find (Key);

      begin
         --  TODO: check if this is okay to do.
         if C = No_Element then
            return;
         end if;

         ESL.Trace.Debug (Message => "Changing state: " & New_State'Img,
                                Context => Context);

         if New_State = CS_DESTROY then
            Storage.Delete (Key => Key);
         else
            Storage.Update_Element (C, Process'Access);
         end if;
      end Change_State;

      function Empty return Boolean is
      begin
         return Storage.Is_Empty;
      end Empty;

      function Get (Key : in Channel_Key) return Channel.Instance is
      begin
         return Storage.Element (Key => Key);
      end Get;

      function Has (Key : in Channel_Key) return Boolean is
      begin
         return Storage.Contains (Key);
      end Has;

      function Image return String is
         use Channel_Storage;
         Buffer : Unbounded_String;
         C      : Cursor := Storage.First;
      begin
         while C /= No_Element loop
            Append (Buffer, Element (C).Image);
            Append (Buffer, ASCII.LF);
            Next (C);
         end loop;

         return To_String (Buffer);
      end Image;

      procedure Insert (Channel : in ESL.Channel.Instance) is
      begin
         Storage.Insert (Key      => Channel.Name,
                         New_Item => Channel);
      end Insert;
   end Synchronized_Storage;

   procedure Change_State (Obj       : in out Instance;
                           Key       : in     ESL.Channel.Channel_Key;
                           New_State : in     ESL.Channel.States) is
   begin
      Obj.Channel_List.Change_State (Key       => Key,
                                     New_State => New_State);
   end Change_State;

   --  TODO
   function Create (Packet : in ESL.Packet.Instance) return Instance is
      pragma Unreferenced (Packet);

   begin
      return Create;
   end Create;

   function Create return Instance is
   begin
      return Obj : Instance;
   end Create;

   function Empty (Obj : in Instance) return Boolean is
   begin
      return Obj.Channel_List.Empty;
   end Empty;

   function Equivalent_Keys (Left  : in Channel_Key;
                             Right : in Channel_Key) return Boolean is
   begin
      --  TODO: Make case insensitive.
      return Left = Right;
   end Equivalent_Keys;

   --  TODO
   function Get (Obj : in Instance;
                 Key : in Channel_Key) return Channel.Instance is
   begin
      return Obj.Channel_List.Get (Key => Key);
   end Get;

   function Has (Obj : in Instance;
                 Key : in Channel_Key) return Boolean is
   begin
      return Obj.Channel_List.Has (Key);
   end Has;

   function Image (Obj : in Instance) return String is
   begin
      return Obj.Channel_List.Image;
   end Image;

   procedure Insert (Obj     : in out Instance;
                     Channel : in     ESL.Channel.Instance) is
      Context : constant String := Package_Name & ".Insert";
   begin
      ESL.Trace.Information (Message => "Inserted channel " & Channel.Image,
                             Context => Context);

      Obj.Channel_List.Insert (Channel => Channel);
   end Insert;

end ESL.Channel.List;
