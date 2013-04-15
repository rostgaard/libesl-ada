with Ada.Finalization;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with GNAT.Sockets;

with ESL.Outbund_Event;
with ESL.Send_Message;

package ESL.Client is
   use ESL;

   Package_Name       : constant String := "ESL.Client";

   Connection_Timeout : exception;
   Not_Connected      : exception;

   type Connection_Event_Handler is not null access procedure;
   --  Parameterless procedure to execute when connection state changes.

   Ignore_Event : constant Connection_Event_Handler;
   --  Silently ignore connection state changes.

   type Instance is tagged limited private;
   --  This is the actual client instance.

   procedure Connect (Client   : in out Instance;
                      Hostname : in     String;
                      Port     : in     Natural);

   procedure Disconnect (Client : in out Instance);

   procedure Authenticate (Obj     : in out Instance;
                           Password : in     String);

   procedure Set_Log_Level (Obj   : in out Instance;
                            Level : in     Natural);

   function Get_Line (Client : in Instance) return String;

   procedure Send_Event (Client : in Instance;
                         Event  : in Outbund_Event.Instance) is null;

   procedure Send_Message (Client : in Instance;
                           Event  : in Send_Message.Instance) is null;

   procedure Send (Client : in Instance;
                   Item   : in String);
   --  Send an abitrary string. Use this as a last resort, as most should be
   --  available through AMI.Packet.Action.

   --  procedure Send (Client : in Instance;
   --                  Item   : in AMI.Packet.Action.Request);
   --  --  Primary send function.

   --  function Send (Client : in Instance;
   --                 Item   : in AMI.Packet.Action.Request)
   --                    return AMI.Parser.Packet_Type;
   --  --  Synchronous version of send operation. Uses an internal buffer to
   --  --  achieve synchronous operation.

   function Is_Connected (Client : in Instance) return Boolean;
   pragma Obsolescent (Is_Connected, "Not supported by GNAT.Sockets.");

   function Connected (Client : in Instance) return Boolean;

   procedure Wait_For_Connection (Client  : in Instance;
                                  Timeout : in     Duration := 3.0);
   --  Waits for a client to establish a connection for duration.
   --  Raises TIMEOUT if the connection is not established within the
   --  given duration.

   type Reference is access Instance;

private

   --  function Read_Packet (Client : access AMI.Client.Instance)
   --                        return AMI.Parser.Packet_Type;

   procedure Ignore is null;
   Ignore_Event : constant Connection_Event_Handler := Ignore'Access;

   type Instance_Handle is new Natural range 1 .. 10;

   function Create return Reference;

   type Instance is new Ada.Finalization.Limited_Controlled with
      record
         Initialized           : Boolean := False;
         Connected             : Boolean := False;
         Server_Greeting       : Ada.Strings.Unbounded.Unbounded_String;
         Authenticated         : Boolean := False;
         Shutdown              : Boolean := False;
         Socket                : GNAT.Sockets.Socket_Type :=
           GNAT.Sockets.No_Socket;
         Channel               : GNAT.Sockets.Stream_Access := null;
         On_Connect_Handler    : Connection_Event_Handler := Ignore_Event;
         On_Disconnect_Handler : Connection_Event_Handler := Ignore_Event;
      end record;

   overriding procedure Initialize (Obj : in out Instance);
   overriding procedure Finalize (Obj : in out Instance);

   procedure Deallocate is new Ada.Unchecked_Deallocation
     (Object => Instance,
      Name   => Reference);

end ESL.Client;
