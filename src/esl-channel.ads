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

with Ada.Containers;
with Ada.Strings.Unbounded;

with ESL.Packet;
with ESL.Channel_Variable.List;

package ESL.Channel is
   use Ada.Strings.Unbounded;

   Package_Name : constant String := "ESL.Channel";

   Unknown_State : exception;

   type States is (CS_NEW, CS_INIT, CS_ROUTING, CS_SOFT_EXECUTE,
                   CS_EXECUTE, CS_EXCHANGE_MEDIA, CS_PARK, CS_CONSUME_MEDIA,
                   CS_REPORTING, CS_DESTROY,
                   CS_HIBERNATE, CS_RESET, CS_HANGUP, CS_DONE);
   --  Channel states.

   function Value (Item : in String) return States;
   --  Converts a string to States type. Raises Unknown_State upon failure.

   function Description (Item : in States) return String;
   --  Gives a human-readable description of what happens with a state.

   subtype Channel_Key is Unbounded_String;

   function Value (Item : in String) return Channel_Key;

   type Instance is tagged record
      Name      : Channel_Key;
      State     : States;
      Variables : ESL.Channel_Variable.List.Instance;
   end record;
   --  Representation of a channel.

   function Create (Packet : in ESL.Packet.Instance) return Instance;
   --

   function Create return Instance;
   --  Empty object instance;

   function Image (Obj : in Instance) return String;

   procedure Add_Variable (Obj      : in out Instance;
                           Variable : in     Channel_Variable.Instance);

private
   use Ada.Containers;

   function Hash (Item : in Channel_Key) return Hash_Type;

   Description_State_Map : constant array (States) of access constant String :=
     (CS_NEW            =>
         new String'("Channel is newly created"),
      CS_INIT           =>
         new String'("Channel Has Been Initialized"),
      CS_ROUTING        =>
         new String'("Channel is Looking for An Extension To Execute"),
      CS_SOFT_EXECUTE   =>
         new String'("Channel is ready to execute from 3rd party control"),
      CS_EXECUTE        =>
         new String'("Channel is executing it's dialplan"),
      CS_EXCHANGE_MEDIA =>
         new String'("Channel is exchanging media with another channel."),
      CS_PARK           =>
         new String'("Channel is accepting media awaiting commands."),
      CS_CONSUME_MEDIA  =>
         new String'("Channel is consuming all media and dropping it."),
      CS_HIBERNATE      =>
         new String'("Channel is in a sleep state"),
      CS_RESET          =>
         new String'("Channel is in a reset state"),
      CS_REPORTING      =>
         new String'("The channel is already hung up, media is already down,"&
          " and now it's time to do any sort of reporting processes" &
          " such as CDR logging."),
      CS_HANGUP         =>
          new String'("Channel is flagged for hangup and ready to end"),
      CS_DESTROY        =>
         new String'("Channel is ready to be destroyed and out of the "&
          " state machine. Memory pools are returned to the core and " &
          "utilized memory from the channel is freed."),
      CS_DONE           =>
         new String'("Channel is ready to be destroyed " &
                     "and out of the state machine"));
   --  Description table used by the Description function.

end ESL.Channel;
