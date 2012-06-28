package AT_Strings is
   Ok               : constant String := "OK";
   Error            : constant String := "ERROR";
   Busy             : constant String := "BUSY";
   No_Carrier       : constant String := "NO CARRIER";
   Echo_Status      : constant String := "ATE?";
   Echo_Reply       : constant String := "E";
   Echo_Set         : constant String := "ATE=";
   Echo_Values      : constant String := "ATE=?";
   PIN_Status       : constant String := "AT+CPIN?";
   PIN_Set          : constant String := "AT+CPIN=";
   PIN_Values       : constant String := "AT+CPIN=?";
   Indicator_Status : constant String := "AT+CIND?";
   Indicator_Reply  : constant String := "+CIND:";
   
end AT_Strings;
