with Ada.Text_IO; use Ada.Text_IO;
with Debug;

package body WWAN_Commands is


   function Local_Echo (WWAN_Card : access Serial_Port) return Boolean is
      Buffer  : String (1 .. 2048) := (others => ' ');
      Filled  : Natural            := 0;
      Got_Ate : Boolean            := False;
      Got_Ok  : Boolean            := False;
   begin
      Debug.Log("Local_Echo TX: ATE?",Debug.Information);
      UART_IO.Put_Line (WWAN_Card, "ATE?");
	 if Index (Source => Translate
		     (Buffer(Buffer'First ..Buffer'First+Filled),
		      Upper_Case_Map),
            Pattern => "ATE?") /= 0 then
         Got_Ate := True;
	elsif Index (Source => Translate
		     (Buffer(Buffer'First ..Buffer'First+Filled),
		      Upper_Case_Map),
            Pattern => "OK") /= 0 then
                    Got_Ok := True;
         end if;

      return Got_Ok or Got_Ate;
   end Local_Echo;

   procedure Set_Local_Echo (WWAN_Card : access Serial_Port;
                             New_Value : Boolean) is

      Buffer  : String (1 .. 2048) := (others => ' ');
      Filled  : Natural            := 0;
      Got_Ate : Boolean            := False;
      Got_Ok  : Boolean            := False;
   begin
      Debug.Log("Local_Echo TX: ATE?",Debug.Information);
      UART_IO.Put_Line (WWAN_Card, "ATE?");
	 if Index (Source => Translate
		     (Buffer(Buffer'First ..Buffer'First+Filled),
		      Upper_Case_Map),
            Pattern => "ATE?") /= 0 then
         Got_Ate := True;
	elsif Index (Source => Translate
		     (Buffer(Buffer'First ..Buffer'First+Filled),
		      Upper_Case_Map),
            Pattern => "OK") /= 0 then
                    Got_Ok := True;
         end if;

      return Got_Ok or Got_Ate;
   end Set_Local_Echo;


   -- AT+CGDCONT=1,"IP","proxy"
   procedure Configure_APN (APN : in String) is
   begin
      raise NOT_IMPLEMENTED;
   end Configure_APN;

   -- AT*ENAP=1,1
   -- AT*ENAP=1,X where X is the index of the Internet Account (as reported by
   -- the AT*EIAC? command or created by the AT*EIAPSW). The command starts the
   -- connection (creates the PDP context based on given parameters of Internet
   -- Account). In practice, after calling the command, the link on the
   -- ethernet interface should be reoprted as up, i.e. available for dhclient
   -- to be used on it.
   -- AT*ENAP?: returns the status of current PS bearer connections.
   -- The value is either 1 if the ethernet link is up, 0 if it is not.
   -- AT*ENAP=0: stops current ethernet emulation, i.e. sets the link down.
   procedure Initiate_Connection is
   begin
      raise NOT_IMPLEMENTED;
   end Initiate_Connection;

   function Get_Mode (WWAN_Card : access Serial_Port) return Card_Mode_Type is
      Locked : Boolean := True;
      Count  : Natural            := 0;
      Buffer : String (1 .. 2048) := (others => ' ');
      Filled : Natural            := 0;
      Mode   : Card_Mode_Type     := Undefined;
   begin
      Debug.Log("Get_mode TX: " & "AT+CFUN?",Debug.Information);
      UART_IO.Put_Line (WWAN_Card,"AT+CFUN?");
      loop
	 exit when Count = 5;
	 UART_IO.Get_Line ((WWAN_Card),Buffer,Filled);
	 Debug.Log("Get_mode RX: " & Buffer (Buffer'First .. Filled) ,Debug.Information);
	 if Index (Source => Translate
		     (Buffer(Buffer'First ..Buffer'First+Filled),
		      Upper_Case_Map),
		   Pattern => "+CFUN:") /= 0 then
--	    Debug.Log("Get_mode: got mode " & Buffer (Buffer'First+7) ,Debug.Information);
	    Mode := Mode_Of(Buffer (Buffer'First+7));
	 elsif Index (Source => Translate
			(Buffer(Buffer'First ..Buffer'First+Filled),
			 Upper_Case_Map),
		      Pattern => "ERROR") /= 0 then
	    raise COMMUNICATION_ERROR;
	 end if;
	 Count := Count+1;
      end loop;
      return Mode;
   end Get_Mode;

   -- AT+CFUN=1
   procedure Set_Mode (WWAN_Card : access Serial_Port; Mode : in Card_Mode_Type) is
      Buffer : String (1 .. 2048) := (others => ' ');
      Filled : Natural            := 0;
   begin
      if Get_Mode(WWAN_Card) = Mode then
	 Debug.Log("Set_mode: card is already in the mode, exiting",Debug.Debug);
	 return;
      end if;

      -- TODO: When the card mode is set to 1, it must wait for a *EMWI: 1,0 \n \n+PACSP0,
      -- which appears to be coming in as an event

      Debug.Log("Set_mode RX: " & "AT+CFUN=" & Char_Of(Mode),Debug.Information);
      UART_IO.Put_Line (WWAN_Card,"AT+CFUN=" & Char_Of(Mode));
      UART_IO.Get_Line ((WWAN_Card),Buffer,Filled);
      Debug.Log("Set_mode RX: "& Buffer(Buffer'First .. Filled) ,Debug.Debug);
      UART_IO.Get_Line ((WWAN_Card),Buffer,Filled);
      Debug.Log("Set_mode RX: "& Buffer(Buffer'First .. Filled) ,Debug.Debug);

   end Set_Mode;

   -- AT*EPIN?
   procedure Remaining_Attempts (WWAN_Card : access Serial_Port) is
   begin
      UART_IO.Put_Line (WWAN_Card,"AT*EPIN?");
      UART_IO.Skip_Line (WWAN_Card);
      UART_IO.Skip_Line (WWAN_Card);
   end Remaining_Attempts;

   procedure Unlock (WWAN_Card : access Serial_Port;
		     PIN       : in String) is
   begin
      -- PUK can be entered with the following : AT+CPIN="PUK","New_PIN"

      Debug.Log ("Unlock: Sending AT+CPIN=""" & Pin & """",Debug.Information);

      -- Push the PIN code
      UART_IO.Put_Line (WWAN_Card,"AT+CPIN=""" & Pin & """");
      UART_IO.Skip_Line (WWAN_Card);
      UART_IO.Skip_Line (WWAN_Card);
   end Unlock;

   --  procedure Detect_State is
   --  begin
   --     WWAN_Card.Sim_Locked := Pin_Locked;
   --  end Detect_State;

   --  procedure Print_State(WWAN_Card : in WWAN_Card_Type) is
   --  begin
   --     Detect_State;
   --     if WWAN_Card.Sim_Locked then
   --  	 Ada.Text_IO.Put_Line ("SIM card is locked");
   --     else
   --  	 Ada.Text_IO.Put_Line ("SIM card is unlocked");
   --     end if;
   --  end Print_State;


   -- Get GPS info via NMEA protocol. First, you have to configure the a few
   -- NMEA options. It is done by sending AT*E2GPSCTL=1,5,1
   -- TODO: Extend this to wait for reply
   procedure Start_GPS (WWAN_Card : access Serial_Port) is
   begin
      UART_IO.Put_Line(WWAN_Card,"AT*E2GPSCTL=1,5,1");
   end Start_GPS;


   function PIN_Locked (WWAN_Card : access Serial_Port) return Boolean is
      Locked : Boolean := True;
      Count  : Natural := 0;
      Buffer : String (1 .. 2048)       := (others => ' ');
      Filled : Natural                  := 0;
   begin
      Put_Line("PIN_Locked: sending AT+CPIN?" );
      UART_IO.Put_Line (WWAN_Card,"AT+CPIN?");

      loop
	 exit when Count = 3;
	 UART_IO.Get_Line ((WWAN_Card),Buffer,Filled);
	 Debug.Log ("PIN_Locked: " & Translate
		    (Buffer(Buffer'First ..Buffer'First+Filled),
		     Upper_Case_Map),Debug.Information);
	 if Index (Source => Translate
		     (Buffer(Buffer'First ..Buffer'First+Filled),
		      Upper_Case_Map),
		   Pattern => "+CPIN: SIM PIN") /= 0 then
	    -- We don't really care, as standard is locked
	    null;
	    -- Put_Line("PIN_Locked: locked: " & Buffer(Buffer'First .. Filled));
	 elsif Index (Source => Translate
		     (Buffer(Buffer'First ..Buffer'First+Filled),
		      Upper_Case_Map),
		      Pattern => "+CPIN: READY") /= 0 then
	    Locked := False;
	    --Put_Line("PIN_Locked: unlocked: " & Buffer(Buffer'First .. Filled));
	 elsif Index (Source => Translate
		     (Buffer(Buffer'First ..Buffer'First+Filled),
		      Upper_Case_Map),
		      Pattern => "ERROR") /= 0 then
	    return False;
	 elsif Index (Source => Translate
		     (Buffer(Buffer'First ..Buffer'First+Filled),
		      Upper_Case_Map),
		      Pattern => "NO CARRIER") /= 0 then
	    return False;
	 elsif Index (Source => Translate
		     (Buffer(Buffer'First ..Buffer'First+Filled),
		      Upper_Case_Map),
		      Pattern => "+CPIN: SIM PUK") /= 0 then
	    raise PUK_REQUIRED;
	 end if;
	 Count := Count+1;
      end loop;
      return Locked;
   end PIN_Locked;
end WWAN_Commands;
