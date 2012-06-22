with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with GNAT.Serial_Communications; use GNAT.Serial_Communications;

-- Local
with UART_IO;

package WWAN_Commands is
   
   PUK_REQUIRED    : exception;
   NOT_IMPLEMENTED : exception;
   
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
      WCDMA_Only); -- 6 WCDMA only (GSM radio off)
   
  
   procedure Set_Mode (WWAN_Card : access Serial_Port; Mode : in Card_Mode_Type);
   
   procedure Unlock(WWAN_Card : access Serial_Port;
		    PIN       : in String);
   
   
   procedure Start_GPS(WWAN_Card : access Serial_Port);
   
   function PIN_Locked(WWAN_Card : access Serial_Port) return Boolean;
   
private 
   Char_Of : constant array (Card_Mode_Type) of Character := 
     (Power_Off  => '0',
      Active     => '1',
      TX_Off     => '2', 
      RX_Off     => '3',
      Power_Save => '4',
      GSM_Only   => '5',
      WCDMA_Only => '6');

end WWAN_Commands;
