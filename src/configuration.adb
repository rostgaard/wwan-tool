with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Configuration is
   use Ada.Text_IO, Ada.Strings.Fixed;

   package Associative_Map is new Ada.Containers.Ordered_Maps(Unbounded_String,
                                                              Unbounded_String);
   use Associative_Map;

   Config            : Map;

   procedure Load_Config_File(Config_File  : in  String) is

      Config_Cursor     : Cursor;
      Success           : Boolean;
      File              : File_Type;
      Line_Length       : Natural;
      Max_String_Length : constant := 255;
      Output_String     : String (1 .. Max_String_Length);
      Index_List        : array(1..256) of Natural;
      Next_Index        : Positive := 1;
      Counter           : Positive := 1;

   begin
      Put_Line("Configuration: opening config file """ &Config_File &"""");

      Open( File => File,
            Mode => In_File,
            Name => Config_File );

      while not End_Of_File(File) loop
         Get_Line(File => File,
                  Item => Output_String,
                  Last => Line_Length);

         -- check if the line is not a comment
         if Output_String (1) /= '#' then

            for I in 1 .. Line_Length loop
               if Output_String (I) = '=' then

                  Config.Insert(To_Unbounded_String(Output_String (1 .. I-1)),
                                To_Unbounded_String
                                  (Output_String (I+1 .. Line_Length)));


                  Counter := Counter+1;
               end if;
            end loop;
         end if;
      end loop;
   end Load_Config_File;

   function Value (Key : in String) return String is
   begin
      return To_String(Config.Element(To_Unbounded_String(key)));
   end Value;

end Configuration;
