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

with Ada.Finalization;
with Ada.Unchecked_Deallocation;
with Ada.Streams.Stream_IO;

private with GNAT.Sockets;

with ESL.Outbund_Event;
with ESL.Send_Message;
with ESL.Command;
with ESL.Channel.List;

package ESL.Client is
   use ESL;

   Package_Name       : constant String := "ESL.Client";

   Connection_Timeout : exception;
   Not_Connected      : exception;

   type Event_Formats is (Plain, XML, JSON);

   type Connection_Event_Handler is not null access procedure;
   --  Parameterless procedure to execute when connection state changes.

   Ignore_Event : constant Connection_Event_Handler;
   --  Silently ignore connection state changes.

   type Instance (On_Connect_Handler    : Connection_Event_Handler;
                  On_Disconnect_Handler : Connection_Event_Handler)
     is tagged limited private;
   --  This is the actual client instance.

   procedure Connect (Client   : in out Instance;
                      Hostname : in     String;
                      Port     : in     Natural);

   procedure Disconnect (Client : in out Instance);

   function Image (Client : in Instance) return String;

   procedure Authenticate (Client   : in out Instance;
                           Password : in     String);

   procedure Set_Log_Level (Obj   : in out Instance;
                            Level : in     Natural);
   --  Corresponds to "log".

   function Receive (Client : in Instance;
                     Count  : in Natural) return String;
   --  Receives _exactly_ Count characters (bytes) from the client channel.
   --  Blocks until the data is available.

   function Get_Line (Client : in Instance) return String;

   procedure Skip_Until_Empty_Line (Obj : in Instance);
   --  Fast-forwards until after the first occurence of an empty line.

   procedure Send_Event (Client : in Instance;
                         Event  : in Outbund_Event.Instance) is null;

   procedure Send_Message (Client : in Instance;
                           Event  : in Send_Message.Instance) is null;

   procedure Send (Client : in Instance;
                   Item   : in String);
   --  Send an abitrary string. Use this as a last resort, as most should be
   --  available via ESL.Command.*
   pragma Obsolescent
     (Send, "To be superseded by ""api"" and ""bgapi "" calls");

   procedure Send (Client : in Instance;
                   Item   : in ESL.Command.Instance'Class);
   --  Primary send function.
   pragma Obsolescent
     (Send, "To be superseded by ""api"" and ""bgapi "" calls");

   procedure API (Client  : in Instance;
                  Command : in ESL.Command.Instance'Class);
   --  Synchronously sends an API command.

   procedure Background_API (Client  : in     Instance;
                             Command : in     ESL.Command.Instance'Class);
   --  Asynchronously sends an API command. A reply is still returned, but
   --  only for the purpose of comfirming that the command was sent.

   function Connected (Client : in Instance) return Boolean;
   --  Returns the last known connection state.

   procedure Wait_For_Connection (Client  : in Instance;
                                  Timeout : in     Duration := 3.0);
   --  Waits for a client to establish a connection for duration.
   --  Raises TIMEOUT if the connection is not established within the
   --  given duration.

   type Reference is access all Instance;

   function Stream (Obj : in Instance)
                    return Ada.Streams.Stream_IO.Stream_Access;

   function "=" (Left, Right : in Reference) return Boolean;

   function Channel_List (Obj : in Instance) return Channel.List.Reference;

private
   use GNAT.Sockets;

   procedure Ignore is null;
   Ignore_Event : constant Connection_Event_Handler := Ignore'Access;

   type Instance_Handle is new Natural range 1 .. 10;

   function Create
     (On_Connect_Handler    : in Connection_Event_Handler;
      On_Disconnect_Handler : in Connection_Event_Handler)
      return Reference;

   type Instance
     (On_Connect_Handler    : Connection_Event_Handler;
      On_Disconnect_Handler : Connection_Event_Handler)
        is new Ada.Finalization.Limited_Controlled with
      record
         Connecting            : Boolean := False;
         Initialized           : Boolean := False;
         Connected             : Boolean := False;
         Authenticated         : Boolean := False;
         Channels              : Channel.List.Reference
           := new Channel.List.Instance;
         Socket                : GNAT.Sockets.Socket_Type :=
           GNAT.Sockets.No_Socket;
         Channel               : GNAT.Sockets.Stream_Access := null;
         Selector              : aliased GNAT.Sockets.Selector_Type;
         Sequence              : Natural := 0;
      end record;

   overriding procedure Initialize (Obj : in out Instance);
   overriding procedure Finalize (Obj : in out Instance);

   procedure Deallocate is new Ada.Unchecked_Deallocation
     (Object => Instance,
      Name   => Reference);

   type Known_Commands is (Help, API, BGAPI, Event, Log);

end ESL.Client;
