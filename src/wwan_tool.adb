with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Exceptions;
with GNAT.Serial_Communications;
with GNAT.OS_Lib;  use GNAT.OS_Lib;

-- Local packages
with WWAN_Commands; use WWAN_Commands;

-- TODO
-- * Make the program take command line arguments (Ada.Command_Line)
-- * Daemonize the program (optional)
-- * Make a call-home feature (check if there is internet already)
-- * Implement the scanning functions
-- * Build a html form/ssh server that receives the reports - or, send sms'
-- * Enable the anti-theft feature to be enabled from sms

procedure WWAN_Tool is 
   use Ada.Text_IO;
   use Ada.Integer_Text_IO;
   use GNAT.Serial_Communications;
   
   Control_Device_Name    : constant String(1 .. 12) := "/dev/ttyACM0";
   GPS_Device_Name        : constant String(1 .. 12) := "/dev/ttyACM2";
   PIN                    : constant String(1 .. 4)  := "6749"; -- Fill in pin here
   Control_Device_Handle  : aliased Serial_Port;
   -- GPS_Device_Handle      : aliased Serial_Port;
begin
   
   GNAT.Serial_Communications.Open (Port => Control_Device_Handle, 
				    Name => Port_Name(Control_Device_Name));
   GNAT.Serial_Communications.Set 
     (Port => Control_Device_Handle, 
      Rate => GNAT.Serial_Communications.B115200, 
      Bits => GNAT.Serial_Communications.CS8, 
      Parity => GNAT.Serial_Communications.None,
      Block => True);
   --	, Timeout => 3.0);          
   
   if PIN_Locked(Control_Device_Handle'Access) then
      Put_Line("Card is locked, trying to unlock");
      Unlock(Control_Device_Handle'Access,PIN);
   else
      Put_Line("Card is unlocked.");
   end if;
   
   -- Print_State(WWAN_Card);
   
exception
   when E : GNAT.SERIAL_COMMUNICATIONS.SERIAL_ERROR =>
      Put_Line("Error communicating with serial device (check presence and permissions)");
end WWAN_Tool;
