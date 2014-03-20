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

--  An ESL.Reply is an utility object representing both an API reply and and
--  the more specific Command/Response.

with ESL.Packet;
with ESL.UUID;
private with Ada.Strings.Unbounded;

package ESL.Reply is

   Package_Name : constant String := "ESL.Reply";

   Okay_Indicator  : constant Character := '+';
   Error_Indicator : constant Character := '-';
   Okay_String     : constant String    := Okay_Indicator  & "OK";
   Error_String    : constant String    := Error_Indicator & "ERR";

   type Kinds is (Invalid, Command_Reply, API_Response);

   type Instance is tagged private;

   type Responses is (Invalid, Error, OK);

   type Errors is (Unknown, No_Answer, No_Channel); --  TODO.

   function Create (Packet : in ESL.Packet.Instance) return Reply.Instance;

   function Job_UUID (Reply : in Instance) return ESL.UUID.Instance;

   function Channel_UUID (Reply : in Instance) return ESL.UUID.Instance;

   function Response (Reply : in Instance) return Responses;

   function Error_Type (Reply : in Instance) return Errors;

   function Response_Body (Reply : in Instance) return String;

   function Image (Reply : in Instance) return String;

   function Null_Reply return Instance;
   pragma Inline (Null_Reply);

private
   use Ada.Strings.Unbounded;

   type Instance is tagged
      record
         Kind     : Kinds := Invalid;
         Job_UUID : ESL.UUID.Instance;
         Raw_Body : Unbounded_String;
      end record;

end ESL.Reply;
