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
with ESL.Packet_Keys;
with ESL.Packet_Content_Type;

package body ESL.Reply is

   function Create (Packet : in ESL.Packet.Instance) return Instance is
      use ESL.Packet_Keys;
      use ESL.Packet_Content_Type;

      Object : Reply.Instance := Null_Reply;
   begin
      if not Packet.Is_Response then
         raise Constraint_Error with "Cannot create reply from event!";
      end if;

      if Packet.Content_Type = API_Response then
         Object.Response := To_Unbounded_String (Packet.Payload);
      else
         Object.Response := To_Unbounded_String
           (Packet.Header.Field (Key => Reply_Text).Value);
      end if;

      if Packet.Header.Contains (Key => Job_UUID) then
         declare
            UUID_String : String renames
              Packet.Header.Field (Key => Job_UUID).Value;
         begin
            Object.UUID := ESL.UUID.Create (Item => UUID_String);
         end;
      end if;

      if Object = Null_Reply then
         raise Constraint_Error;
      end if;

      return Object;
   end Create;

   function Image (Reply : in Instance) return String is
   begin
      return To_String (Reply.Response) & " " & Reply.UUID.Image;
   end Image;

   function Response (Reply : in Instance) return Responses is
      Response_String : String renames To_String (Reply.Response);
   begin
      if Response_String (Response_String'First) = '+' then
         return OK;
      elsif Response_String (Response_String'First) = '-' then
         return Error;
      elsif Response_String'Length = 0 then
         return Null_Response;
      end if;

      return Undefined;
   end Response;

   function Response_Body (Reply : in Instance) return String is
   begin
      return To_String (Source => Reply.Response);
   end Response_Body;

   function UUID (Reply : in Instance) return ESL.UUID.Instance is
   begin
      return Reply.UUID;
   end UUID;

end ESL.Reply;
