with Ada.Text_IO;

package Debug is
   use Ada.Text_IO;

   type Severity_Type is (Debug, Information, Warning, Error, Critical, Fatal);

   type Transport_Map_Type is array (Severity_Type'Range) of File_Type;
   type Filename_Map_Type is array (Severity_Type'Range) of access String;

   procedure Set_Logfile (Filename : access String;
                           Severity : in Severity_Type);

   function Get_Logfile (Severity : in Severity_Type) return String;

   procedure Log (Message  : in String;
                  Severity : in Severity_Type );

   procedure Reload_File(Severity : in Severity_Type);

private
   Logfile : access String;
   Logfile_Handle : File_Type;
   Transport_Map : Transport_Map_Type :=
     ( Information => Standard_Output,
       Warning => Standard_Output,
       others  => Standard_Error );


   Filename_Map : Filename_Map_Type :=
     ( Information => new String'("Standard_Output"),
       Warning => new String'("Standard_Output"),
       others  => new String'("Standard_Error") );
end Debug;
