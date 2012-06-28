with Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with GNAT.Serial_Communications;

-- Local packages
with WWAN_Commands; use WWAN_Commands;
with Configuration;
with Debug;

procedure WWAN_Tool is
   use Ada.Text_IO;
   use GNAT.Serial_Communications;
   
   Control_Device_Handle  : aliased Serial_Port;
   Card_Status            : Indicator_Status_Type;
   -- GPS_Device_Handle      : aliased Serial_Port;
      
   Buffer : String (1 .. 512);
   Offset : Natural := 0;
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
      Parity => GNAT.Serial_Communications.None,
      Block  => True,
      Timeout => 1.0);

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
        ("WWAN_Tool: Local echo is off", Debug.Debug);
   end if;


   if PIN_Locked(Control_Device_Handle'Access) then
      Debug.Log
   	("WWAN_Tool: Card is locked, trying to unlock",Debug.Information);
      Unlock(Control_Device_Handle'Access,Configuration.Value("PIN"));
   else
      Debug.Log("WWAN_Tool: Card is unlocked.",Debug.Information);
      Set_Mode(Control_Device_Handle'Access,Active);
   end if;
   
   Update_Status(Control_Device_Handle'Access,Card_Status);
   Print_Status(Card_Status);
   
   -- Enter main loop;
   Read_Line_Loop (Control_Device_Handle'Access);   
   
exception
   when Error : GNAT.SERIAL_COMMUNICATIONS.SERIAL_ERROR =>
      Debug.Log
	("Error communicating with serial device (check presence and permissions) " & Exception_Information(Error),
	 Debug.Fatal);
   when Error : others =>
      Debug.Log
	("Got undefined exception: " & Exception_Information(Error),Debug.Fatal);
      
end WWAN_Tool;
