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

with Ada.Strings.Fixed;

with ESL.Packet_Content_Type,
     ESL.Packet_Keys,
     ESL.Trace;

package body ESL.Reply is

   function Trim_Newlines (Item : in String) return String;

   --------------------
   --  Channel_UUID  --
   --------------------

   function Channel_UUID (Reply : in Instance) return ESL.UUID.Instance is
      use Ada.Strings;
   begin
      case Reply.Kind is
         when Invalid .. Command_Reply =>
            raise Constraint_Error with
              "Only Command/Reply contain Channel Information.";
         when API_Response =>
            if Reply.Response /= OK then
               raise Constraint_Error with
                 "Reply errors do not contain channel information.";
            else
               declare
                  Position : constant Natural :=
                    Reply.Response_Body'First + Okay_String'Length;
                  Buffer   : String renames Reply.Response_Body
                    (Position .. Reply.Response_Body'Last);
               begin
                  return ESL.UUID.Create (Trim_Newlines
                                          (Fixed.Trim (Source => Buffer,
                                                       Side   => Both)));
               end;
            end if;
      end case;
   end Channel_UUID;

   --------------
   --  Create  --
   --------------

   function Create (Packet : in ESL.Packet.Instance) return Instance is
      use ESL.Packet_Keys;
      use ESL.Packet_Content_Type;

      Object : Reply.Instance := Null_Reply;
   begin

      if Packet.Is_Event then
         raise Constraint_Error with "Cannot create reply from event!";
      end if;

      if Packet.Content_Type = API_Response then
         Object.Kind     := API_Response;
         Object.Raw_Body := To_Unbounded_String (Packet.Raw_Payload);
      elsif Packet.Content_Type = Command_Reply then
         Object.Kind     := Command_Reply;
         Object.Raw_Body := To_Unbounded_String
           (Packet.Header.Field (Key => Reply_Text).Value);
      end if;

      if Packet.Header.Contains (Key => Job_UUID) then
         declare
            UUID_String : String renames
              Packet.Header.Field (Key => Job_UUID).Value;
         begin
            Object.Job_UUID := ESL.UUID.Create (Item => UUID_String);
         end;
      end if;

      if Object = Null_Reply then
         raise Constraint_Error;
      end if;

      return Object;
   end Create;

   ------------------
   --  Error_Type  --
   ------------------

   function Error_Type (Reply : in Instance) return Errors is
      use Ada.Strings;

      Context : constant String := Package_Name & ".Error_Type";

   begin
      case Reply.Kind is
         when Invalid =>
            raise Constraint_Error with
              "Only Command/Reply contain error information.";
         when Command_Reply .. API_Response =>
            if Reply.Response /= Error then
               raise Constraint_Error with
                 "Tried to extract error from non-error reply.";
            else
               declare
                  Position : constant Natural :=
                    Reply.Response_Body'First + Error_String'Length;
                  Buffer   : String renames Reply.Response_Body
                    (Position .. Reply.Response_Body'Last);
               begin
                  return Errors'Value (Trim_Newlines
                                       (Fixed.Trim (Source => Buffer,
                                                   Side   => Both)));
               exception
                  when Constraint_Error =>
                     ESL.Trace.Error (Message => "Unknown error type" & Buffer,
                                      Context => Context);
                     return Unknown;
               end;
            end if;
      end case;
   end Error_Type;

   -------------
   --  Image  --
   -------------

   function Image (Reply : in Instance) return String is
   begin
      return
        "Kind: "    & Reply.Kind'Img & "; " &
        "Job_UUID:" & Reply.Job_UUID.Image & "; " &
        "Raw_Body:" & To_String (Reply.Raw_Body);
   end Image;

   ----------------
   --  Job_UUID  --
   ----------------

   function Job_UUID (Reply : in Instance) return ESL.UUID.Instance is
   begin
      if Reply.Kind /= Command_Reply then
         raise Constraint_Error with "No Job UUID attached!";
      end if;

      return Reply.Job_UUID;
   end Job_UUID;

   ------------------
   --  Null_Reply  --
   ------------------

   function Null_Reply return Instance is
   begin
      return (Kind     => <>,
              Job_UUID => ESL.UUID.Null_UUID,
              Raw_Body => Null_Unbounded_String);
   end Null_Reply;

   ----------------
   --  Response  --
   ----------------

   function Response (Reply : in Instance) return Responses is
      Response_String : String renames To_String (Reply.Raw_Body);
   begin
      if Response_String (Response_String'First) = Error_Indicator then
         return Error;
      elsif Response_String (Response_String'First) = Okay_Indicator then
         return OK;
      else
         case Reply.Kind is
            when Invalid .. Command_Reply =>
               return Error;
               --  Every response should have an ok or error indicator.
            when API_Response =>
               return OK;
               --  As the API commands may deliver arbitrary information,
               --  we have to assume all went well up to this point, and
               --  let functions futher down the consumption chain detemine
               --  wheter or not there was an error.
         end case;
      end if;
   end Response;

   ---------------------
   --  Response_Body  --
   ---------------------

   function Response_Body (Reply : in Instance) return String is
   begin
      return To_String (Reply.Raw_Body);
   end Response_Body;

   ---------------------
   --  Trim_Newlines  --
   ---------------------

   function Trim_Newlines (Item : in String) return String is
      Trim_Count : Natural := 0;
   begin
      for Index in reverse Item'Range loop
         exit when
           Item (Index) /= ASCII.LF and
           Item (Index) /= ASCII.CR;
         Trim_Count := Trim_Count + 1;
      end loop;

      return Item (Item'First .. Item'Last - Trim_Count);
   end Trim_Newlines;

end ESL.Reply;
