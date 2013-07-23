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

with ESL.Packet;
with ESL.UUID;

package ESL.Reply is

   Package_Name : constant String := "ESL.Reply";

   type Instance is tagged private;

   type Responses is (Null_Response, Error, Timeout, OK);

   function Create (Packet : in ESL.Packet.Instance) return Reply.Instance;

   function UUID (Reply : in Instance) return ESL.UUID.Instance;

   function Response (Reply : in Instance) return Responses;

   function Image (Reply : in Instance) return String;

   Null_Reply : constant Reply.Instance;

private
   type Instance is tagged
      record
         UUID     : ESL.UUID.Instance;
         Response : Responses;
      end record;

   Null_Reply : constant Reply.Instance := (UUID     => ESL.UUID.Null_UUID,
                                            Response => Null_Response);

end ESL.Reply;
