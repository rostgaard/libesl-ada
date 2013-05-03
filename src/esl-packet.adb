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

   ------------------
   --  Add_Header  --
   ------------------

   procedure Add_Header (Obj   :     out Instance;
                         Field : in      Packet_Field.Instance) is
      Context : constant String := Package_Name & ".Add_Header";
   begin

      if Field = Empty_Line then
         ESL.Trace.Debug (Context => Context,
                          Message => "Skipping empty line");
         return;
      end if;

      ESL.Trace.Debug (Context => Context,
                       Message => "Adding " & Field.Image);

      if Field.Key = Content_Type then
         Obj.Content_Type := Packet_Content_Type.Create (Field.Value);
      else
         Obj.Headers.Insert (Key      => Field.Key,
                             New_Item => Field);
      end if;
   exception
      when others =>
         ESL.Trace.Error (Context => Context,
                          Message => "Error Adding " & Field.Image);
         raise;
   end Add_Header;

   --------------------
   --  Add_Variable  --
   --------------------

   procedure Add_Variable (Obj      : in out Instance;
                           Variable : in     Packet_Variable.Instance) is
      Context : constant String := Package_Name & ".Add_Variable";
   begin

      Obj.Variables.Insert (Key      => Variable.Name,
                            New_Item => Variable);
      ESL.Trace.Debug
        (Message => "Adding variable: " & Packet_Variable.Image (Variable),
         Context => Context);

   exception
      when others =>
         ESL.Trace.Error (Context => Context,
                          Message => "Error Adding " &
                            Packet_Variable.Image (Variable));
   end Add_Variable;

   ----------------------
   --  Content_Length  --
   ----------------------

   function Content_Length (Obj : in Instance) return Natural is
   begin
      if Obj.Headers.Contains (Key => Content_Length) then
         return Natural'Value
           (Obj.Headers.Element (Key => Content_Length).Value);
      else
         return 0;
      end if;
   end Content_Length;

   --------------
   --  Create  --
   --------------

   function Create return Instance is
   begin
      return (Content_Type => Packet_Content_Type.Null_Instance,
              Headers      => Header_Storage.Empty_Map,
              Payload      => Payload_Storage.Empty_Map,
              Variables    => Variable_Storage.Empty_Map);
   end Create;

   -----------------------
   --  Equivalent_Keys  --
   -----------------------

   function Equivalent_Keys (Left  : in Packet_Keys.Event_Keys;
                             Right : in Packet_Keys.Event_Keys) return Boolean
   is
   begin
      return Left = Right;
   end Equivalent_Keys;

   -----------------------
   --  Equivalent_Keys  --
   -----------------------

   function Equivalent_Keys (Left  : in Unbounded_String;
                             Right : in Unbounded_String) return Boolean is
   begin
      return Left = Right;
   end Equivalent_Keys;

   ------------------
   --  Has_Header  --
   ------------------

   function Has_Header (Obj : in Instance;
                        Key : in Packet_Keys.Event_Keys) return Boolean is
      use Packet_Content_Type;
   begin
      return Obj.Headers.Contains (Key => Key);
   end Has_Header;

   -------------------
   --  Hash_Header  --
   -------------------

   function Hash_Header (Item : in Packet_Keys.Event_Keys) return
     Ada.Containers.Hash_Type is
   begin
      return Packet_Keys.Event_Keys'Pos (Item);
   end Hash_Header;

   -------------
   --  Image  --
   -------------

   function Image (Obj : in Instance) return String is
   begin
      return "Content_Type:" & Obj.Content_Type.Image & ASCII.LF & ASCII.LF &
        "Headers:" & ASCII.LF & Image (Obj.Headers) & ASCII.LF & ASCII.LF &
        "Payload:" & ASCII.LF & Image (Obj.Payload) & ASCII.LF & ASCII.LF &
        "Variables:" & ASCII.LF & Image (Obj.Variables);
   end Image;

   function Image (List : Header_Storage.Map) return String is
      use Header_Storage;

      Buffer : Unbounded_String;
   begin
      for C in List.Iterate loop
         Append (Buffer, (Key (C)'Img));
         Append (Buffer, ": ");
         Append (Buffer, Element (C).Value);
         Append (Buffer, ASCII.LF);
      end loop;
      return To_String (Buffer);
   end Image;

   function Image (List : Payload_Storage.Map) return String is
      use Payload_Storage;
      Buffer : Unbounded_String;
   begin
      for C in List.Iterate loop
         Append (Buffer, Element (C).Key'Img);
         Append (Buffer, ": ");
         Append (Buffer, Element (C).Value);
         Append (Buffer, ASCII.LF);
      end loop;
      return To_String (Buffer);
   end Image;

   function Image (List : Variable_Storage.Map) return String is
      use Variable_Storage;
      Buffer : Unbounded_String;
   begin
      for C in List.Iterate loop
         Append (Buffer, Key (C));
         Append (Buffer, ": ");
         Append (Buffer, Element (C).Value);
         Append (Buffer, ASCII.LF);
      end loop;
      return To_String (Buffer);
   end Image;

   ------------------------
   --  Payload_Contains  --
   ------------------------

   function Payload_Contains (Obj : in Instance;
                              Key : in Packet_Keys.Event_Keys) return Boolean
   is
   begin
      return Obj.Payload.Contains (Key => Key);
   end Payload_Contains;

   ---------------------
   --  Payload_Field  --
   ---------------------

   function Payload_Field (Obj : in Instance;
                           Key : in Packet_Keys.Event_Keys)
                           return Packet_Field.Instance is
   begin
      return Obj.Payload.Element (Key => Key);
   end Payload_Field;

   ----------------------------
   --  Process_And_Add_Body  --
   ----------------------------

   procedure Process_And_Add_Body (Obj      : in out Instance;
                                   Raw_Data : in     String) is
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
            when ASCII.LF => --  Seen a full line.
               declare
                  Field : ESL.Packet_Field.Instance;
                  Line  : String renames
                    Linebuffer (Linebuffer'First .. Position - 1);
                  Variable_String : constant String := "variable_";
               begin
                  if Line'Length > Variable_String'Length and then
                    Line (Variable_String'Range) = Variable_String then
                     --  declare
                     --  Variable : Packet_Variable.Instance := Parse_Line (
                     --  begin
                     --  Obj.Variables.Insert (Parse_Line
                     Obj.Add_Variable
                       (Variable => Parse_Line (Line
                        (Variable_String'Length + 1 .. Line'Last)));
                  else

                     Field := Parse_Line (Item => Line);
                     ESL.Trace.Debug
                       (Message => "Processing line: " &
                          URL_Utilities.Decode (Field.Value),
                        Context => Context);

                     if Field = Empty_Line then
                        if Obj.Payload_Contains (Key => Content_Length) then
                           --  Skip the payload.
                           --  TODO:
                           return;
                        end if;

                     elsif Field.Key /= Unknown then

                        Obj.Payload.Insert (Key      => Field.Key,
                                            New_Item => Field);
                     end if;

                  end if;
               end;
               Position := Raw_Data'First;
            when others =>
               Linebuffer (Position) := Raw_Data (I);
               Position := Position + 1;
         end case;
      end loop;
   end Process_And_Add_Body;
end ESL.Packet;
