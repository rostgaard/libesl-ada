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

with Ada.Containers.Hashed_Maps;
private with Ada.Strings.Unbounded.Hash;

with ESL.Packet;

package ESL.Channel_Variable.List is

   type Instance is tagged private;

   type Reference is access all Instance;

   Not_Found : exception;

   function Create (Packet : in ESL.Packet.Instance) return Instance;

   function Create return Instance;

   function Image (Obj : in Instance) return String;

   function Get (Obj : in Instance;
                 Key : in String) return String;
   --  Use the keys from ESL.Channel_Variable.Keys, or arbitrary string.

   procedure Add_Variable (Obj      : in out Instance;
                           Variable : in     Channel_Variable.Instance);

private

   function Equivalent_Keys (Left  : in Unbounded_String;
                             Right : in Unbounded_String) return Boolean;

   package Variable_Storage is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => Channel_Variable.Instance,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => Equivalent_Keys,
      "="             => Channel_Variable."=");

   type Instance is tagged
      record
         Storage : Variable_Storage.Map;
      end record;

end ESL.Channel_Variable.List;
