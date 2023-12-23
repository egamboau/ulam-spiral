with Ada.Strings;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Parse_Args;


with Main;

procedure ulam_spiral is
   AP : Parse_Args.Argument_Parser;
   size : Integer;
begin



   AP.Append_Positional (O => Parse_Args.Make_Positive_Option, Name => "N");
   AP.Set_Prologue ("Prints Ulam spiral on screen. " &
    " Logs to stderr so output can be redirected");
   AP.Parse_Command_Line;

   if AP.Parse_Success then
      size := AP.Integer_Value ("N");
      declare
         package Ulam_Entry_Point is new Main(array_size => size);
         current_spiral: Ulam_Entry_Point.Ulam_Spiral_Access;
      begin
         current_spiral := Ulam_Entry_Point.calculate_spiral;
         Ada.Text_IO.Put_Line (Ada.Strings.Unbounded.To_String
           (Source => Ulam_Entry_Point.Join_Array (current_spiral)));
      end;
   else
      Ada.Text_IO.Put_Line ("Error while parsing command-line arguments: ");
      Ada.Text_IO.Put_Line (AP.Parse_Message);
   end if;
end ulam_spiral;
