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

with ESL.UUID;
with ESL.Packet;

private with Ada.Containers.Ordered_Maps;

package ESL.Job.List is

   Package_Name : constant String := "ESL.Job.List";

   Not_Done        : exception;
   Timeout_Reached : exception;

   type Instance is tagged limited private;

   procedure Push (List   : in out Instance;
                   Packet : in ESL.Packet.Instance);

   procedure Pop (List : in out Instance;
                  UUID : in     ESL.UUID.Instance;
                  Job  :    out ESL.Job.Instance);

   procedure Discard (List : in out Instance;
                      UUID : in ESL.UUID.Instance);

   procedure Subscribe (List : in out Instance;
                        UUID : in ESL.UUID.Instance);

   procedure Wait_For (List    : in out Instance;
                       UUID    : in     ESL.UUID.Instance;
                       Job     :    out ESL.Job.Instance;
                       Timeout : in     Duration := 2.0);

   function Reply_Ready (List : in Instance;
                         UUID : in ESL.UUID.Instance) return Boolean;

   function Image (List : in Instance) return String;

private
   use ESL.UUID;

   package Job_Storage is new Ada.Containers.Ordered_Maps
     (Key_Type     => ESL.UUID.Instance,
      Element_Type => ESL.Job.Instance);

   protected type Synchronized_Storage is
      procedure Push (Packet : in ESL.Packet.Instance);
      procedure Pop  (UUID : in     ESL.UUID.Instance;
                      Job  :    out ESL.Job.Instance);
      procedure Subscribe (UUID : in ESL.UUID.Instance);
      procedure Discard (UUID : in ESL.UUID.Instance);
      function Image return String;
      function Reply_Ready (UUID : in ESL.UUID.Instance) return Boolean;

   private
      function Has_Reply (UUID : in ESL.UUID.Instance) return Boolean;
      Claimed   : Job_Storage.Map;
      Unclaimed : Job_Storage.Map;
   end Synchronized_Storage;

   type Instance is tagged limited
      record
         Storage : Synchronized_Storage;
      end record;

end ESL.Job.List;
