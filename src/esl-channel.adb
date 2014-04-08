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

with ESL.Packet_Keys,
     ESL.Trace,
     ESL.Unbounded_Case_Insensitive_Hash;

package body ESL.Channel is

   --------------------
   --  Add_Variable  --
   --------------------

   procedure Add_Variable (Obj      : in out Instance;
                           Variable : in     Channel_Variable.Instance) is
   begin
      Obj.Variables.Add_Variable (Variable => Variable);
   end Add_Variable;

   --------------
   --  Create  --
   --------------

   function Create return Instance is
      Obj : constant Instance := (others => <>);
   begin
      return Obj;
   end Create;

   --------------
   --  Create  --
   --------------

   function Create (Packet : in ESL.Packet.Instance) return Instance is
      use ESL.Packet_Keys;
   begin
      return (Name      => Value (Packet.Field (Key => Unique_ID).Value),
              State     => Value (Packet.Field (Key => Channel_State).Value),
              Variables => Channel_Variable.List.Create);
   exception
      when Unknown_State =>
         ESL.Trace.Error (Message => "Unknown_State:",
                          Context => "Create");
         raise;
   end Create;

   -------------------
   --  Description  --
   -------------------

   function Description (Item : in States) return String is
   begin
      return Description_State_Map (Item).all;
   end Description;

   ------------
   --  Hash  --
   ------------

   function Hash (Item : in Channel_Key) return Hash_Type is
   begin
      return Unbounded_Case_Insensitive_Hash (Item);
   end Hash;

   -------------
   --  Image  --
   -------------

   function Image (Obj : in Instance) return String is
   begin
      return To_String (Obj.Name) & ", " &
        Obj.State'Img &  ", " & Obj.Variables.Image;
   end Image;

   -------------
   --  Value  --
   -------------

   function Value (Item : in String) return Channel_Key is
   begin
      return Channel_Key (To_Unbounded_String (Item));
   end Value;

   -------------
   --  Value  --
   -------------

   function Value (Item : in String) return States is
      Context : constant String := Package_Name & ".Value (String -> States)";
   begin
      return States'Value (Item);
   exception
      when Constraint_Error =>
         ESL.Trace.Error (Message => "Invalid value: " & Item,
                          Context => Context);
         raise Unknown_State;
   end Value;

end ESL.Channel;
