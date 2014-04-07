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
with ESL.Parsing_Utilities;

package body ESL.Packet is
   use Packet_Keys;

   ----------------
   --  Contains  --
   ----------------

   function Contains (Obj : in Instance;
                      Key : in Packet_Keys.Event_Keys) return Boolean
   is
   begin
      case Obj.Header.Content_Type is
         when Null_Value | Text_Disconnect_Notice |
              Auth_Request | API_Response =>
            return False;

         when Text_Event_Plain =>
            return Obj.Payload.Contains (Key => Key);

         when Command_Reply =>
            return Obj.Payload.Contains (Key => Key);

         when Text_Event_XML =>
            raise Program_Error with "Not implemented";

         when Text_Event_JSON =>
            raise Program_Error with "Not implemented";
      end case;

   end Contains;

   ----------------------
   --  Content_Length  --
   ----------------------

   function Content_Length (Obj : in Instance) return Natural is
   begin
      return Obj.Header.Content_Length;
   end Content_Length;

   function Content_Type (Obj : in Instance) return Content_Types is
   begin
      return Obj.Header.Content_Type;
   end Content_Type;

   --------------
   --  Create  --
   --------------

   function Create return Instance is
      Obj : Instance;
   begin
      return Obj;
   end Create;

   --------------------
   --  Empty_Packet  --
   --------------------

   function Empty_Packet return Instance is
   begin
      return (Header    => Packet_Header.Empty_Header,
              Raw_Body  => Null_Unbounded_String,
              Payload   => Payload_Storage.Empty_Map,
              Variables => Channel_Variable.List.Create);
   end Empty_Packet;

   -----------------------
   --  Equivalent_Keys  --
   -----------------------

   function Equivalent_Keys (Left  : in Unbounded_String;
                             Right : in Unbounded_String) return Boolean is
   begin
      return Left = Right;
   end Equivalent_Keys;

   function Equivalent_Keys (Left  : in Packet_Keys.Event_Keys;
                             Right : in Packet_Keys.Event_Keys) return Boolean
   is
   begin
      return Left = Right;
   end Equivalent_Keys;

   function Event (Obj : in Instance) return Packet_Keys.Inbound_Events is
      Context   : constant String := Package_Name & ".Event";
      Not_Event : exception;
   begin
      if not Is_Event (Obj => Obj) then
         raise Not_Event;
      end if;

      return Packet_Keys.Inbound_Events'Value
        (Obj.Field (Key => Event_Name).Value);

   exception
      when Not_Event =>
         ESL.Trace.Error (Message => "Packet accessed erroneusly as event",
                          Context => Context);
         raise Constraint_Error with "Packet is not an event";
      when Constraint_Error =>
         ESL.Trace.Error (Message => "Unknown Event name : " &
                            Obj.Payload.Element (Key => Event_Name).Value,
                          Context => Context);
         raise Constraint_Error with  "Unknown Event name : " &
                            Obj.Payload.Element (Key => Event_Name).Value;

   end Event;

   -------------
   --  Field  --
   -------------

   function Field (Obj : in Instance;
                   Key : in Packet_Keys.Event_Keys)
                   return Packet_Field.Instance is
   begin
      if not Obj.Payload.Contains (Key) then
         raise Constraint_Error with "No value for key " & Key'Img;
      end if;

      case Obj.Header.Content_Type is
         when Null_Value =>
            raise Constraint_Error with "Null packet";

         when Text_Disconnect_Notice |
              Auth_Request | API_Response |
              Command_Reply =>
            raise Constraint_Error with "Use payload instead";

         when Text_Event_Plain =>
            return Obj.Payload.Element (Key => Key);

         when Text_Event_XML =>
            raise Program_Error with "Not implemented";

         when Text_Event_JSON =>
            raise Program_Error with "Not implemented";
      end case;

   end Field;

   ------------------
   --  Hash_Field  --
   ------------------

   function Hash_Field (Item : in Packet_Keys.Event_Keys) return
     Ada.Containers.Hash_Type is
   begin
      return Packet_Keys.Event_Keys'Pos (Item);
   end Hash_Field;

   -------------
   -- Header  --
   -------------

   function Header (Obj : in Instance) return ESL.Packet_Header.Instance is
   begin
      return Obj.Header;
   end Header;

   -------------
   --  Image  --
   -------------

   function Image (Obj : in Instance) return String is
   begin
      return "Content_Type:" & Image (Obj.Header.Content_Type) &
        ASCII.LF & ASCII.LF &
        "Headers:" & ASCII.LF &
        Packet_Header.Image (Obj.Header) & ASCII.LF & ASCII.LF &
        "Payload:" & ASCII.LF & Obj.Raw_Payload;
   end Image;

   function Image (List : Payload_Storage.Map) return String is
      use Payload_Storage;
      Buffer : Unbounded_String;
      C      : Cursor := List.First;
   begin
      while C /= No_Element loop
         Append (Buffer, Element (C).Key'Img);
         Append (Buffer, ": ");
         Append (Buffer, Element (C).Value);
         Append (Buffer, ASCII.LF);

         Next (C);
      end loop;
      return To_String (Buffer);
   end Image;

   function Is_Event (Obj : in Instance) return Boolean is
   begin
      return Obj.Contains (Key => Event_Name);
   end Is_Event;

   function Is_Response (Obj : in Instance) return Boolean is
   begin
      return Obj.Header.Content_Type = API_Response or
        Obj.Header.Content_Type = Command_Reply;
   end Is_Response;

   function Other_Leg (Obj : in Instance) return ESL.UUID.Instance is
   begin
      if Obj.Contains (Other_Leg_Unique_ID) then
         return ESL.UUID.Create (Obj.Field (Other_Leg_Unique_ID).Value);
      else
         return ESL.UUID.Null_UUID;
      end if;
   end Other_Leg;

   ----------------------------
   --  Process_And_Add_Body  --
   ----------------------------

   procedure Process_And_Add_Body (Obj      : in out Instance;
                                   Raw_Data : in     String) is
      use Parsing_Utilities;

      Linebuffer : String (Raw_Data'Range) := (others => ASCII.NUL);
      Position   : Natural := Raw_Data'First;
   begin

      Obj.Raw_Body := To_Unbounded_String (Raw_Data);

      if Obj.Header.Content_Type = API_Response then
         --  We do not process further on the response body when encountering
         --  an API response as these do not have a unified format.
         return;
      elsif Obj.Header.Content_Type = Text_Disconnect_Notice then
         --  The disconnect notices merely contain a cleartext information
         --  about going to ClueCon, so we ignore these as well.
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
                     declare
                        Variable_Line : String renames  Line
                          (Variable_String'Length + 1 .. Line'Last);
                        Variable : constant Channel_Variable.Instance :=
                          Parse_Line (Variable_Line);
                     begin
                        Obj.Variables.Add_Variable (Variable);
                     end;
                  else

                     Field := Parse_Line (Item => Line);

                     if Field = Empty_Line then
                        if Obj.Contains (Key => Content_Length) then
                           --  Skip the payload.
                           --  TODO:
                           return;
                        end if;

                     elsif Field.Key /= Unknown then

                        begin
                           Obj.Payload.Insert (Key      => Field.Key,
                                               New_Item => Field);
                        exception
                           when Constraint_Error =>
                              null; -- Ignore duplicates.
                        end;
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

   -------------------
   --  Push_Header  --
   -------------------

   procedure Push_Header (Obj   :    out Instance;
                          Field : in    Header_Field.Instance) is
   begin
      Obj.Header.Add_Header (Field => Field);
   end Push_Header;

   ---------------
   --  Payload  --
   ---------------

   function Raw_Payload (Obj : in Instance) return String is
   begin
      return To_String (Obj.Raw_Body);
   end Raw_Payload;

   ----------------
   --  Subevent  --
   ----------------

   function Subevent (Obj : in Instance) return String is
   begin
      if Obj.Contains (Event_Subclass) then
         return Obj.Field (Event_Subclass).Decoded_Value;
      else
         return "";
      end if;
   end Subevent;

   ------------
   --  UUID  --
   ------------

   function UUID (Obj : in Instance) return ESL.UUID.Instance is
   begin
      if Obj.Contains (Unique_ID) then
         return ESL.UUID.Create (Obj.Field (Unique_ID).Value);
      else
         return ESL.UUID.Null_UUID;
      end if;
   end UUID;

   -----------------
   --  Variables  --
   -----------------

   function Variables (Obj : in Instance)
                       return Channel_Variable.List.Instance is
   begin
      return Obj.Variables;
   end Variables;

end ESL.Packet;
