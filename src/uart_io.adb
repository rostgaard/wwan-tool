with Ada.Text_IO; use Ada.Text_Io;
with Ada.Streams; use Ada.Streams;

package body UART_IO is

   procedure Get_Line (Source : access Ada.Streams.Root_Stream_Type'Class;
                       Item   :    out String;
                       Last   :    out Natural) is
      Char   : Character;
      Buffer : Stream_Element_Array (1 .. 1);
      Offset : Stream_Element_Offset;      
   begin
      Last := Item'First - 1;
      loop
         exit when Last >= Item'Last;
	 Source.Read (Buffer, Offset);
	 
         --Char := Character'Input (Source);
	 Char := Character'Val (Integer (Buffer (1)));
         case Char is
            when ASCII.CR =>
               null;
            when ASCII.LF =>
               return;
            when others =>
               Last := Last + 1;
               Item (Last) := Char;
         end case;
      end loop;
      Put_Line
	("UART received"
	   & Stream_Element_Offset'Image (Offset)
	   & " bytes");
   end Get_Line;

   procedure Put_Line (Target : access Ada.Streams.Root_Stream_Type'Class;
		       Item   : in     String) is
   begin
      for Index in Item'Range loop
         Character'Write (Target,
                           Item (Index));
      end loop;
      -- The protocol waits for a CR
      Character'Write (Target,
			ASCII.CR);

   end Put_Line;

   procedure Skip_Line (Source : access Ada.Streams.Root_Stream_Type'Class) is
      Char : Character;
   begin
      loop
         Char := Character'Input (Source);
         case Char is
            when ASCII.CR =>
               null;
            when ASCII.LF =>
               return;
	    when others =>
	       null;
         end case;
      end loop;
   end Skip_Line;
end UART_IO;
