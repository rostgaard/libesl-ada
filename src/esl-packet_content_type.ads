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

   type Subtypes is (Unknown, Request, Event_Plain, Reply, Response);

   type Instance is tagged private;

   function Create (Item : in String) return Instance;

   function Image (Item : in Instance) return String;

   function "=" (Left  : in Instance;
                 Right : in Instance) return Boolean;

   Null_Instance : constant Instance;

   --  Objects for use in comparisons.
   Auth_Request : constant Instance;

private

   type Instance is tagged record
      Base : Types;
      Sub  : Subtypes;
   end record;

   Null_Instance : constant Instance := (Base => Unknown,
                                         Sub  => Unknown);

   Auth_Request : constant Instance := (Base => Auth,
                                        Sub  => Request);
end ESL.Packet_Content_Type;
