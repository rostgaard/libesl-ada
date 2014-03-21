-------------------------------------------------------------------------------
--                                                                           --
--                     Copyright (C) 2014-, AdaHeads K/S                     --
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

with Ada.Streams.Stream_IO;

private with GNAT.Sockets;

with ESL.Outbund_Event;
with ESL.Send_Message;
with ESL.Command;
with ESL.Reply;
with ESL.Packet_Keys;

package ESL.Basic_Client is
   use ESL;

   Package_Name       : constant String := "ESL.Basic_Client";

   Connection_Timeout     : exception;
   Command_Failed         : exception;
   Not_Connected          : exception;
   Authentication_Failure : exception;

   type Event_Formats is (Plain, XML, JSON);

   type States is (Created,
                   Connecting, Connected,
                   Disconnecting, Disconnected,
                   Finalizing, Finalized);

   type Connection_Event_Handler is not null access procedure;
   --  Parameterless procedure to execute when connection state changes.

   Ignore_Event : constant Connection_Event_Handler;
   --  Silently ignore connection state changes.

   type Instance is tagged limited private;
   --  Client object.

   procedure Connect (Object   : in out Instance;
                      Hostname : in     String;
                      Port     : in     Natural);

   procedure Disconnect (Object   : in out Instance);

   function Image (Client : in Instance) return String;

   procedure Set_Log_Level (Obj   : in out Instance;
                            Level : in     Natural);
   --  Corresponds to "log".

   procedure Unmute_Event (Client : in out Instance;
                           Format : in     Event_Formats;
                           Event  : in     ESL.Packet_Keys.Inbound_Events);
   procedure Unmute_Event (Client : in out Instance;
                           Format : in     Event_Formats;
                           Event  : in     String);
   --  Corresponds to "event".

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

   procedure API (Client  : in out Instance;
                  Command : in     ESL.Command.Instance'Class;
                  Reply   : in out ESL.Reply.Instance);
   --  Synchronously sends an API command.

   procedure Background_API (Client  : in     Instance;
                             Command : in     ESL.Command.Instance'Class);
   --  Asynchronously sends an API command. A reply is still returned, but
   --  only for the purpose of comfirming that the command was sent and
   --  returning a UUID ticket for mapping reply events to the action.

   function State (Client : in Instance) return States;
   --  Returns the current state of the client.

   procedure Wait_For_Connection (Client  : in Instance;
                                  Timeout : in     Duration := 3.0);
   --  Waits for a client to establish a connection for duration.
   --  Raises TIMEOUT if the connection is not established within the
   --  given duration.

   function Stream (Obj : in Instance)
                    return Ada.Streams.Stream_IO.Stream_Access;

   procedure Signal_Disconnect (Obj : out Instance);

   procedure Authenticate (Client   : in out Instance;
                           Password : in     String);

private
   use GNAT.Sockets;

   protected type Mutex is
      entry Seize;
      procedure Release;
   private
      Owned : Boolean := False;
   end Mutex;

   procedure Ignore is null;
   Ignore_Event : constant Connection_Event_Handler := Ignore'Access;

   type Instance_Handle is new Natural range 1 .. 10;

   procedure Change_State (Object    :    out Instance;
                           New_State : in     States);

   type Instance is tagged limited
      record
         Current_State         : States  := Created;
         Authenticated         : Boolean := False;
         Socket                : GNAT.Sockets.Socket_Type :=
           GNAT.Sockets.No_Socket;
         Channel               : GNAT.Sockets.Stream_Access := null;
         Selector              : aliased GNAT.Sockets.Selector_Type;
         Sequence              : Natural := 0;
         IO_Mutex              : Mutex;
      end record;

   type Known_Commands is (Help, API, BGAPI, Event, Log);
end ESL.Basic_Client;
