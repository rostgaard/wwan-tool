with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with GNAT.Serial_Communications; use GNAT.Serial_Communications;

-- Local
with UART_IO;

package WWAN_Commands is

   PUK_REQUIRED        : exception;
   NOT_IMPLEMENTED     : exception;
   COMMUNICATION_ERROR : exception;
   PROTOCOL_ERROR      : exception;
   NO_CARRIER_ERROR    : exception;
   BUSY_ERROR          : exception;
   
   subtype Battery_Charge_Type is Natural range 0 .. 5;
   subtype Signal_Type is Natural range 0 .. 5;
   subtype Call_Setup_Type_Type is Natural range 0 .. 3;
   
   
   type Indicator_Status_Type is
      record
	 Valid             : Boolean              := False;
	 Battery_Charge    : Battery_Charge_Type  := 0;
	 Signal            : Signal_Type          := 0;
	 Battery_Warning   : Boolean              := False;
	 Charger_Connected : Boolean              := False;
	 Service           : Boolean              := False;
	 Sounder           : Boolean              := False;
	 Message           : Boolean              := False;
	 Call              : Boolean              := False;
	 Roam              : Boolean              := False;
	 Smsfull           : Boolean              := False;
	 Call_Setup        : Call_Setup_Type_Type := 0;
	 Call_Held         : Boolean              := False;
      end record;
   
   type Card_Mode_Type is
     (Power_Off,
      -- 0 Minimum functionality,
      --   that is, the phone is turned off. Default value
      Active,
      -- 1 Full functionality, that is, the phone is turned on
      TX_Off,
      -- 2 Disable phone transmit RF circuits only. Not supported
      --   by all WWAN cards
      RX_Off,
      -- 3 Disable phone receive RF circuits only. Not supported
      --   By all WWAN cards
      Power_Save,
      -- 4 Disable phone transmit and receive RF circuits.
      --   Note: This is often referred to as "flight mode"
      GSM_Only,
      -- 5 GSM only (WCDMA radio off)
      WCDMA_Only,
      -- 6 WCDMA only (GSM radio off)
      Undefined);
   
   procedure Read_Line_Loop (WWAN_Card : access Serial_Port);
   
   procedure Update_Status (WWAN_Card        : access Serial_Port; 
			    Indicator_Status :    out Indicator_Status_Type );
   procedure Print_Status (Indicator_Status :    out Indicator_Status_Type );
   
   
   function Local_Echo (WWAN_Card : access Serial_Port) return Boolean;
   procedure Set_Local_Echo (WWAN_Card : access Serial_Port;
                             New_Value : Boolean);


   procedure Set_Mode (WWAN_Card : access Serial_Port; Mode : in Card_Mode_Type);
   function Get_Mode (WWAN_Card : access Serial_Port) return Card_Mode_Type;

   procedure Unlock(WWAN_Card : access Serial_Port;
		    PIN       : in String);


   procedure Start_GPS(WWAN_Card : access Serial_Port);

   function PIN_Locked(WWAN_Card : access Serial_Port) return Boolean;

private
   Char_Of : constant array (Card_Mode_Type) of Character :=
     (Power_Off  => '0',
      Active     => '1',
      -- TX_Off     => '2', -- These are unsupported by the modem
      -- RX_Off     => '3',
      Power_Save => '4',
      GSM_Only   => '5',
      WCDMA_Only => '6',
      others     =>  ASCII.NUL );

   Char_Of_Boolean : constant array (Boolean) of Character :=
                       (True  => '1', False => '0');

   Boolean_Of_Char : constant array (Character range '0' .. '1') of Boolean :=
                       ('0' => True, '1' => False);

   Mode_Of : constant array (Character) of Card_Mode_Type :=
     ( '0' => Power_Off,
       '1' => Active,
       '4' =>  Power_Save,
       '5' => GSM_Only,
       '6' => WCDMA_Only,
      others => Undefined);

end WWAN_Commands;
