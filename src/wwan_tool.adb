with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Exceptions;
with GNAT.Serial_Communications;
with GNAT.OS_Lib;  use GNAT.OS_Lib;

-- Local packages
with WWAN_Commands; use WWAN_Commands;
with Configuration;
with Debug;

procedure WWAN_Tool is
   use Ada.Text_IO;
   use Ada.Integer_Text_IO;
   use GNAT.Serial_Communications;

   Control_Device_Handle  : aliased Serial_Port;
   -- GPS_Device_Handle      : aliased Serial_Port;
begin
   Configuration.Load_Config_File("wwan_tool.conf");

   GNAT.Serial_Communications.Open (Port => Control_Device_Handle,
				    Name => Port_Name
				      (Configuration.Value
					 ("Control_Device_Name")));
   GNAT.Serial_Communications.Set
     (Port   => Control_Device_Handle,
      Rate   => GNAT.Serial_Communications.B115200,
      Bits   => GNAT.Serial_Communications.CS8,
      Parity => GNAT.Serial_Communications.None);
      --  Block  => True,
      --  Timeout => 10.0);

   if Local_Echo (Control_Device_Handle'Access) then
      Debug.Log
        ("WWAN_Tool: Local echo is on", Debug.Debug);
   else
      Debug.Log
        ("WWAN_Tool: Local echo is off", Debug.Debug);
   end if;
   
   Set_Local_Echo (Control_Device_Handle'Access,False);
   
   
   if Local_Echo (Control_Device_Handle'Access) then
      Debug.Log
        ("WWAN_Tool: Local echo is on", Debug.Debug);
   else
      Debug.Log
        ("WWAN_Tool: Local echo is on", Debug.Debug);
   end if;


   if PIN_Locked(Control_Device_Handle'Access) then
      Debug.Log
   	("WWAN_Tool: Card is locked, trying to unlock",Debug.Information);
      Unlock(Control_Device_Handle'Access,Configuration.Value("PIN"));
   else
      Debug.Log("WWAN_Tool: Card is unlocked.",Debug.Information);
      Set_Mode(Control_Device_Handle'Access,Active);
   end if;

exception
   when E : GNAT.SERIAL_COMMUNICATIONS.SERIAL_ERROR =>
      Debug.Log
	("Error communicating with serial device (check presence and permissions)",
	 Debug.Fatal);
end WWAN_Tool;
