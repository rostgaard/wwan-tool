with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with GNAT.Serial_Communications; use GNAT.Serial_Communications;

-- Local
with UART_IO;

package WWAN_Commands is
   
   PUK_REQUIRED : exception;
   
   procedure Unlock(WWAN_Card : access Serial_Port;
		    PIN       : in String);
   
   
   procedure Start_GPS(WWAN_Card : access Serial_Port);
   
   function PIN_Locked(WWAN_Card : access Serial_Port) return Boolean;

end WWAN_Commands;
