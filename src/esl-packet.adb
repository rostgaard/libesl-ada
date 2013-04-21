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

with URL_Utilities;

with ESL.Trace;
with ESL.Parsing_Utilities;

package body ESL.Packet is
   use ESL.Packet_Keys;

   procedure Add_Header (Obj   :     out Instance;
                         Field : in      Packet_Field.Instance) is
      Context : constant String := Package_Name & ".Add_Header";
      pragma Unreferenced (Context);
   begin
      if Field.Key = Content_Type then
         Obj.Content_Type := Packet_Content_Type.Create (Field.Value);
      elsif Field.Key = Content_Length then
         Obj.Content_Length :=  Natural'Value (Field.Value);
      end if;
   end Add_Header;

   function Content_Length (Obj : in Instance) return Natural is
   begin
      return Obj.Content_Length;
   end Content_Length;

   function Equivalent_Keys (Left  : in Packet_Keys.Event_Keys;
                             Right : in Packet_Keys.Event_Keys) return Boolean
   is
   begin
      return Left = Right;
   end Equivalent_Keys;

   function Has_Header (Obj : in Instance;
                        Key : in Packet_Keys.Event_Keys) return Boolean is
      use Packet_Content_Type;
   begin
      if
        Key = Content_Type and
        Obj.Content_Type /= Packet_Content_Type.Null_Instance
      then
         return True;
      elsif Key = Content_Length then
         return Obj.Content_Length > 0;
      end if;

      return False;
   end Has_Header;

   function Hash_Header (Item : in Packet_Keys.Event_Keys) return
     Ada.Containers.Hash_Type is
   begin
      return Packet_Keys.Event_Keys'Pos (Item);
   end Hash_Header;

   function Image (Obj : in Instance) return String is
   begin
      return "Content_Type:" & Obj.Content_Type.Image &
        ", Content_Length:" & Obj.Content_Length'Img;
   end Image;

   ----------------------------
   --  Process_And_Add_Body  --
   ----------------------------

   procedure Process_And_Add_Body (Obj      : in Instance;
                                   Raw_Data : in String) is
      use Packet_Content_Type;
      use Parsing_Utilities;

      Context    : constant String := Package_Name & ".Process_And_Add_Body";

      Linebuffer : String (Raw_Data'Range) := (others => ASCII.NUL);
      Position   : Natural := Raw_Data'First;
   begin
      if Obj.Content_Type = Api_Response then
         ESL.Trace.Information (Message => "Skipping package of type " &
                                  Obj.Content_Type.Image,
                                Context => Context);
         return;
      end if;

      for I in Raw_Data'Range loop
         case Raw_Data (I) is
            when ASCII.CR =>
               null;
            when ASCII.LF =>
               declare
                  Field : ESL.Packet_Field.Instance;
               begin
                  Field := Parse_Line (
                    Item => Linebuffer (Linebuffer'First .. Position - 1));

                  ESL.Trace.Debug
                    (Message => "Processing line: " &
                       URL_Utilities.Decode (Field.Value),
                     Context => "Process_And_Add_Body");
               end;
               Position := Raw_Data'First;
            when others =>
               Linebuffer (Position) := Raw_Data (I);
               Position := Position + 1;
         end case;
      end loop;
   end Process_And_Add_Body;
end ESL.Packet;
