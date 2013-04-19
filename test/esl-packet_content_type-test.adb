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

with Ada.Assertions;

procedure ESL.Packet_Content_Type.Test is
   use ESL;

   Auth_Request     : constant String := "auth/request";
   Text_Event_Plain : constant String := "text/event-plain";
   API_Response     : constant String := "api/response";

   function Two_Way_Conversion (Item : in String) return Boolean;

   function Two_Way_Conversion (Item : in String) return Boolean is
   begin
      return Packet_Content_Type.Create (Item => Item).Image = Item;
   end Two_Way_Conversion;

begin

   Ada.Assertions.Assert (Check   => Two_Way_Conversion (Auth_Request),
                          Message => Auth_Request & " Failed!");

   Ada.Assertions.Assert (Check   => Two_Way_Conversion (Text_Event_Plain),
                          Message => Text_Event_Plain & " Failed!");

   Ada.Assertions.Assert (Check   => Two_Way_Conversion (API_Response),
                          Message => API_Response & " Failed!");

end ESL.Packet_Content_Type.Test;
