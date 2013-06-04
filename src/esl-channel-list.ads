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

with Ada.Containers;
with Ada.Containers.Hashed_Maps;

with ESL.Packet;

package ESL.Channel.List is

   Package_Name : constant String := "ESL.Channel.List";

   type Instance is tagged limited private;

   type Reference is access all Instance'Class;

   Not_Found : exception;

   function Create (Packet : in ESL.Packet.Instance) return Instance;

   function Create return Instance;

   function Image (Obj : in Instance) return String;

   function Get (Obj : in Instance;
                 Key : in Channel_Key) return Channel.Instance;
   --  Use the keys from ESL.Channel_Variable.Keys, or arbitrary string.

   function Empty (Obj : in Instance) return Boolean;

   procedure Insert (Obj     : in out Instance;
                     Channel : in     ESL.Channel.Instance);

   procedure Change_State (Obj       : in out Instance;
                           Key       : in     ESL.Channel.Channel_Key;
                           New_State : in     ESL.Channel.States);

private

   function Equivalent_Keys (Left  : in Channel_Key;
                             Right : in Channel_Key) return Boolean;

   package Channel_Storage is new Ada.Containers.Hashed_Maps
     (Key_Type        => Channel_Key,
      Element_Type    => Channel.Instance,
      Hash            => ESL.Channel.Hash,
      Equivalent_Keys => Equivalent_Keys,
      "="             => Channel."=");

   protected type Synchronized_Storage (Owner : access Instance'Class) is
      procedure Change_State (Key       : in     ESL.Channel.Channel_Key;
                              New_State : in     ESL.Channel.States);
      function Empty return Boolean;
      function Get (Key : in Channel_Key) return Channel.Instance;
      function Image return String;
      procedure Insert (Channel : in ESL.Channel.Instance);
   private
      Storage : Channel_Storage.Map;
   end Synchronized_Storage;

   type Instance is tagged limited
      record
         Channel_List : Synchronized_Storage (Instance'Access);
      end record;

end ESL.Channel.List;
