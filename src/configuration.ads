with Ada.Text_IO, Ada.Strings.Fixed;
package Configuration is
   type Configuration_Keys is
      record
         Key   : String(1 .. 256);
         Value : String(1 .. 256);
      end record;

   procedure Load_Config_File(Config_File  : in  String);
   function Value (Key : in String) return String;
end Configuration;
