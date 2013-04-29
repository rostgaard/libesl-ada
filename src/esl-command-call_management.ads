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

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

package ESL.Command.Call_Management is
   use ESL;
   use Ada.Strings.Unbounded;

   type Instance is tagged private;

   procedure Break is null;
   --  Deprecated. See uuid_break.

   procedure Create_UUID is null;
   --  Creates a new UUID and returns it as a string.
   --  Usage: create_uuid

   type Originate_Options is
     (Group_Confirm_Key,
      Group_Confirm_File,
      Forked_Dial,
      Fail_On_Single_Reject,
      Ignore_Early_Media,
      Return_Ring_Ready,
      Originate_Retries,
      Originate_Retry_Sleep_Ms,
      Origination_Caller_Id_Name,
      Origination_Caller_Id_Number,
      Originate_Timeout,
      Sip_Auto_Answer);

   type Originate_Parameters is array (Natural range <>) of Originate_Options;
   --  TODO: add values.

   procedure Originate (Call_URL         : in String;
                        --  URL you are calling.
                        Extension        : in String;
                        --  Destination number to enter dialplan with
                        Dialplan         : in String := "";
                        --  Which dialplan to perform the lookup in.
                        --  Defaults to 'xml'.
                        Context          : in String := "";
                        --  Defaults to 'default'.
                        Caller_ID_Name   : in String := "";
                        --  CallerID name.
                        Caller_ID_Number : in String := "";
                        --  CallerID number.
                        Timeout          : in Duration;
                        --  Timeout in seconds.
                        Options : Originate_Parameters)
   is null;
   --  Originate a new Call using an extension.

   package Dialplan_Application_Arguments is
     new Ada.Containers.Vectors (Index_Type   => Natural,
                                 Element_Type => Unbounded_String);

   type Dialplan_Application is
      record
         Name      : Unbounded_String;
         Arguments : Dialplan_Application_Arguments.Vector;
      end record;

   procedure Originate (Call_URL         : in String;
                        --  URL you are calling.
                        Appliction        : in Dialplan_Application;
                        --  Destination number to enter dialplan with
                        Dialplan         : in String := "";
                        --  Which dialplan to perform the lookup in.
                        --  Defaults to 'xml'.
                        Context          : in String := "";
                        --  Defaults to 'default'.
                        Caller_ID_Name   : in String := "";
                        --  CallerID name.
                        Caller_ID_Number : in String := "";
                        --  CallerID number.
                        Timeout          : in Duration;
                        --  Timeout in seconds.
                        Options : Originate_Parameters)
   is null;
   --  Originate a new Call using an application.
   --

   procedure Pause (UUID  : in String;
                    On    : in Boolean) is null;
   --  Pause <uuid> media
   --  Usage: pause <uuid> <on|off>

   procedure UUID_Answer (UUID : in String) is null;
   --  Answer a channel
   --  Usage: uuid_answer <uuid>

   type Audio_Ranges is new Integer range -4 .. 4;
   type Audio_Modes is (Read, Write);

   type Audio_Levels (Mute : Boolean) is
      record
         case Mute is
            when False =>
               Level : Audio_Ranges := 0;
            when True =>
               null;
         end case;
      end record;

   procedure UUID_Audio_Start (UUID  : in String;
                               Mode  : in Audio_Modes;
                               Level : in Audio_Levels) is null;
   --  uuid_audio
   --  Adjust the audio levels on a channel or mute (read/write)
   --  via a media bug.
   --  Usage: uuid_audio <uuid> [start [read|write] [mute|level <level>]|stop]
   procedure UUID_Audio_Stop (UUID  : in String) is null;

   procedure UUID_Break (UUID     : in String;
                         Flag_All : in Boolean) is null;
   --  Break out of media being sent to a channel. For example, if an audio
   --  file is being played to a channel, issuing uuid_break will discontinue
   --  the media and the call will move on in the dialplan, script, or
   --  whatever is controlling the call.
   --  Usage: uuid_break <uuid> [all]
   --  If the all flag is used then all audio files/prompts/etc. that are
   --  queued up to be played to the channel will be removed, whereas without
   --  the all flag only the currently playing file will be discontinued.

   procedure UUID_Bridge (UUID       : in String;
                          UUID_Other : in String) is null;
   --  Bridge two call legs together.
   --  Usage: uuid_bridge <uuid> <other_uuid>
   --  uuid_bridge needs atleast any one leg to be answered.

   type Broadcast_Mode is (A_Leg, B_Leg, Both);

   --  TODO: writeup with app.
   procedure UUID_Broadcast (UUID : in String;
                             Path : in String;
                             Mode : in Broadcast_Mode) is null;
   --  Execute an arbitrary dialplan application on a specific uuid.
   --  If a filename is specified then it is played into the channel(s).
   --  To execute an application use "app::args" syntax.
   --  Usage: uuid_broadcast <uuid> <path> [aleg|bleg|both]
   --  Execute an application on a chosen leg(s) with optional
   --  hangup afterwards:
   --  Usage: uuid_broadcast <uuid> app[![hangup_cause]]::args [aleg|bleg|both]

   procedure UUID_Buglist (UUID : in String) is null;
   --  List the media bugs on channel
   --  Usage: uuid_buglist <uuid>

   procedure UUID_Chat (UUID    : in String;
                        Message : in String) is null;
   --  Send a chat message.
   --  usage: <uuid> <text>
   --  If the endpoint associated with the session <uuid> has a receive_event
   --  handler, this message gets sent to that session and is interpreted as
   --  an instant message.

   type Debug_Modes is
     (Read, Write, Both, Video_Read, Video_Write, Video_Both);
   --  TODO: Add read and write formats. See FS Wiki.
   procedure UUID_Debug_Media (UUID : in String;
                               Mode : in Debug_Modes;
                               On   : in Boolean) is null;
   procedure UUID_Deflect (UUID    : in String;
                           SIP_URL : in String) is null;
   --  Deflect an answered SIP call off of FreeSWITCH by
   --  sending the REFER method
   --  Usage: uuid_deflect <uuid> <sip URL>
   --  uuid_deflect waits for the final response from the far end to be
   --  reported. It returns the sip fragment from that response as the text in
   --  the FreeSWITCH response to uuid_deflect. If the far end reports the
   --  REFER was successful, then FreeSWITCH will issue a bye on the channel.

   type Displacement_Actions is (Start, Stop);
   procedure UUID_Displace
     (UUID   : in String;
      Action : in Displacement_Actions;
      File   : in String;
      --  Path to an audio source (wav, shout, etc...).
      Limit  : in Natural := 0;
      --  Number of seconds before terminating the displacement.
      Mux    : in Boolean := False) is null;

   procedure UUID_Display (UUID    : in String;
                           Display : in String) is null;
   --  Updates the display on a phone if the phone supports this.
   --  This works on some SIP phones right now including Polycom and Snom.
   --  usage: <uuid> [<display>]

   --  TODO: Implement with a record.
   procedure UUID_Dual_Transfer (UUID : in String) is null;
   --  Transfer each leg of a call to different destinations.
   --  usage: <uuid> <A-dest-exten>[/<A-dialplan>][/<A-context>]
   --  <B-dest-exten>[/<B-dialplan>][/<B-context>]

private
   type Instance is new Command.Instance with null record;
end ESL.Command.Call_Management;
