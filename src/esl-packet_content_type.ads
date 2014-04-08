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

package ESL.Packet_Content_Type is

   Package_Name : constant String := "ESL.Packet_Content_Type";

   type Types is (Unknown, Auth, Text, Command, API);

   type Subtypes is (Unknown, Request, Event_Plain, Event_JSON, Event_XML,
                     Reply, Response, Disconnect_Notice);

   type Content_Types is (Null_Value,
                          Auth_Request, API_Response,
                          Command_Reply,
                          Text_Event_JSON, Text_Event_Plain,
                          Text_Event_XML,
                          Text_Disconnect_Notice);

   type Composite is tagged private;

   function Value (Item : in String) return Composite;

   function Value (Item : in String) return Content_Types;

   function Value (Item : in Composite) return Content_Types;

   function Value (Item : in Content_Types) return Composite;

   function Image (Item : in Composite) return String;

   function Image (Item : in Content_Types) return String;

   overriding
   function "=" (Left  : in Composite;
                 Right : in Composite) return Boolean;

   Null_Instance : constant Composite;

private

   type Composite is tagged record
      Base : Types;
      Sub  : Subtypes;
   end record;

   Null_Instance : constant Composite := (Base => Unknown,
                                          Sub  => Unknown);
end ESL.Packet_Content_Type;
