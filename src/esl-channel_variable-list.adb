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

package body ESL.Channel_Variable.List is

   --------------------
   --  Add_Variable  --
   --------------------

   procedure Add_Variable (Obj      : in out Instance;
                           Variable : in     Channel_Variable.Instance) is
      Context : constant String := Package_Name & ".Add_Variable";
   begin

      Obj.Storage.Insert (Key      => Variable.Name,
                            New_Item => Variable);
      ESL.Trace.Debug
        (Message => "Adding variable: " & Channel_Variable.Image (Variable),
         Context => Context);

   exception
      when others =>
         ESL.Trace.Error (Context => Context,
                          Message => "Error Adding " &
                            Channel_Variable.Image (Variable));
   end Add_Variable;

   --------------
   --  Create  --
   --------------

   function Create (Packet : in ESL.Packet.Instance) return Instance is
      pragma Unreferenced (Packet);
   begin
      --  TODO: Create the map.
      return (Storage => Variable_Storage.Empty_Map);
   end Create;

   --------------
   --  Create  --
   --------------

   function Create return Instance is
   begin
      return (Storage => Variable_Storage.Empty_Map);
   end Create;

   -----------------------
   --  Equivalent_Keys  --
   -----------------------

   function Equivalent_Keys (Left  : in Unbounded_String;
                             Right : in Unbounded_String) return Boolean is
   begin
      return Left = Right;
   end Equivalent_Keys;

   -----------
   --  Get  --
   -----------

   function Get (Obj : in Instance;
                 Key : in String) return String is
   begin
      if Obj.Storage.Contains (To_Unbounded_String (Key)) then
         return To_String
           (Obj.Storage.Element (To_Unbounded_String (Key)).Value);
      else
         raise Not_Found;
      end if;
   end Get;

   -------------
   --  Image  --
   -------------

   function Image (Obj : in Instance) return String is
      use Variable_Storage;
      Buffer : Unbounded_String;
   begin
      for C in Obj.Storage.Iterate loop
         Append (Buffer, Key (C));
         Append (Buffer, ": ");
         Append (Buffer, Element (C).Value);
         Append (Buffer, ASCII.LF);
      end loop;
      return To_String (Buffer);
   end Image;

end ESL.Channel_Variable.List;
