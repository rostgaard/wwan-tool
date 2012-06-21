with Ada.Streams;
package UART_IO is
   -- A generic IO function from JSA's stream shortcuts.
   procedure Get_Line (Source : access Ada.Streams.Root_Stream_Type'Class;
                       Item   :    out String;
                       Last   :    out Natural);
   
   -- This one is more specific as it appends CR at the end of the string
   procedure Put_Line (Target : access Ada.Streams.Root_Stream_Type'Class;
		       Item   : in     String);
   
   -- Skips a line
   procedure Skip_Line (Source : access Ada.Streams.Root_Stream_Type'Class);
end UART_IO;
