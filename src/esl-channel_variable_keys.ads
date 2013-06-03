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

package ESL.Channel_Variable_Keys is

   Prefix                 : constant String := "variable_";
   --  Prefix for all variable keys.

   Direction              : constant String := Prefix & "direction";
   UUID                   : constant String := Prefix & "uuid";
   Call_UUID              : constant String := Prefix & "call_uuid";
   Session_ID             : constant String := Prefix & "session_id";
   SIP_From_User          : constant String := Prefix & "sip_from_user";
   SIP_From_URI           : constant String := Prefix & "sip_from_uri";
   SIP_From_Host          : constant String := Prefix & "sip_from_host";
   Channel_Name           : constant String := Prefix & "channel_name";
   SIP_Call_Id            : constant String := Prefix & "sip_call_id";
   SIP_Local_Network_Addr : constant String :=
     Prefix & "sip_local_network_addr";
   SIP_Network_IP         : constant String := Prefix & "sip_network_ip";
   SIP_Network_Port       : constant String := Prefix & "sip_network_port";
   SIP_Received_IP        : constant String := Prefix & "sip_received_ip";
   SIP_Received_Port      : constant String := Prefix & "sip_received_port";
   SIP_Via_Protocol       : constant String := Prefix & "sip_via_protocol";
   SIP_From_User_Stripped : constant String
     := Prefix & "sip_from_user_stripped";
   SIP_From_Tag           : constant String := Prefix & "sip_from_tag";
   Sofia_Profile_Name     : constant String := Prefix & "sofia_profile_name";
   Recovery_Profile_Name  : constant String
     := Prefix & "recovery_profile_name";
   SIP_Remote_Party_ID    : constant String := Prefix & "sip_Remote-Party-ID";
   SIP_CID_Type           : constant String := Prefix & "sip_cid_type";
   SIP_Full_Via           : constant String := Prefix & "sip_full_via";
   SIP_From_Display       : constant String := Prefix & "sip_from_display";

end ESL.Channel_Variable_Keys;
