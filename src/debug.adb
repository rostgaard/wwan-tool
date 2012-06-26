with Ada.Text_IO;

package body Debug is

   procedure Log (Message  : in String;
                  Severity : in Severity_Type ) is
   begin
      case Severity is
         when Debug =>
            Put_Line (Transport_Map(Debug), "Debug: " &Message);
         when Information =>
            Put_Line (Transport_Map(Information), "Information: " &Message);
         when Warning =>
            Put_Line (Transport_Map(Warning), "Warning: " &Message);
         when Error =>
            Put_Line (Transport_Map(Error), "Error: " &Message);
         when Critical =>
            Put_Line (Transport_Map(Critical), "Critical: " &Message);
         when Fatal =>
            Put_Line (Transport_Map(Fatal), "Fatal: " &Message);
      end case;
   end Log;

   procedure Set_Logfile (Filename : access String;
                           Severity : in Severity_Type) is
   begin
--        Filename_Map(Severity) := Filename;
--        if Filename.all = "Standard_Output" then
--           Transport_Map(Severity) := Standard_Output;
--        elsif Filename.all = "Standard_Error" then
--           Transport_Map(Severity) := Standard_Error;
--        else
         Open (Name => Filename.all,
               Mode => Out_File,
               File => Transport_Map(Severity));
--      end if;
   end Set_Logfile;

   function Get_Logfile(Severity : in Severity_Type) return String is
   begin
      return Filename_Map(Severity).all;
   end Get_Logfile;

   procedure Reload_File(Severity : in Severity_Type) is
   begin
      Open (Name => Logfile.all,
            Mode => Out_File,
            File => Logfile_Handle);
   end;
end Debug;
