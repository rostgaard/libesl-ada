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

      ESL.Trace.Debug (Message => "Creating Reply from packet:" & Packet.Image,
                       Context => "ESL.Reply.Create");

      if Packet.Content_Type = API_Response then
         declare
            Reply_String : String renames Packet.Payload;
         begin
            ESL.Trace.Debug (Message => Reply_String,
                             Context => "ESL.Reply.Create");
            if Reply_String (Reply_String'First) = '+' then
               Object.Response := OK;
            elsif Reply_String (Reply_String'First) = '-' then
               Object.Response := Error;
            end if;
         end;
      elsif Packet.Header.Contains (Key => Reply_Text) then
         declare
            Reply_String : String renames
              Packet.Header.Field (Key => Reply_Text).Value;
         begin
            ESL.Trace.Debug (Message => Reply_String,
                             Context => "ESL.Reply.Create");
            if Reply_String (Reply_String'First) = '+' then
               Object.Response := OK;
            elsif Reply_String (Reply_String'First) = '-' then
               Object.Response := Error;
            end if;
         end;
      end if;

      if Packet.Contains (Key => Job_UUID) then
         declare
            UUID_String : String renames
              Packet.Field (Key => Job_UUID).Decoded_Value;
         begin
            Object.UUID := ESL.UUID.Create (Item => UUID_String);
         end;
      end if;

      ESL.Trace.Debug (Message => "Created:" & Object.Image,
                       Context => "ESL.Reply.Create");

      if Object = Null_Reply then
         raise Constraint_Error;
      end if;

      return Object;
   end Create;

   function Image (Reply : in Instance) return String is
   begin
      return Reply.Response'Img & " " & Reply.UUID.Image;
   end Image;

   function Response (Reply : in Instance) return Responses is
   begin
      return Reply.Response;
   end Response;

   function UUID (Reply : in Instance) return ESL.UUID.Instance is
   begin
      return Reply.UUID;
   end UUID;

end ESL.Reply;
