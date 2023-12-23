with Ada.Strings;

package body Main is

   --   Variables for program
   quotient : constant Integer := array_size / 2;
   reminder : constant Integer := array_size mod 2;
   x_center : constant Integer :=
     (if reminder = 0 then quotient - 1 else quotient);
   y_center : constant Integer := quotient;

   Current_Number_Queue : Data_Queue.Queue;

   result_array : Ulam_Spiral_Access;

   --   body declarations
   protected body Result_Array_Access is separate;
   task body Ulam_Task_Queue is separate;

   --   Functions for program
   function Join_Array (To_Join : Ulam_Spiral_Access) return Ada.Strings.Unbounded.Unbounded_String is
      result : Ada.Strings.Unbounded.Unbounded_String;
      first: boolean := True;
      CurrentRecordElement: Ulam_Result;
   begin
      for row in To_Join'Range(1) loop
         if not first then
            Ada.Strings.Unbounded.Append (Source => result, New_Item => Character'Val (10));
         end if;
         for Character in To_Join'Range(2) loop
            CurrentRecordElement := To_Join(row,Character);
            if CurrentRecordElement.is_prime then
               Ada.Strings.Unbounded.Append (Source => result, New_Item => '*');
            else
               Ada.Strings.Unbounded.Append (Source => result, New_Item => '-');
            end if;
         end loop;
         first := False;
      end loop;
      return result;
   end Join_Array;

   function calculate_spiral return Ulam_Spiral_Access is
   begin

      declare
         task_pool : array (Thread_Index) of Ulam_Task_Queue;
      begin
         --  enqueue the elements
         for Number_To_Process in Process_Numbers loop
            Current_Number_Queue.Enqueue
              (New_Item =>
                 (number     => Number_To_Process, x_position => x_center,
                  y_position => y_center, is_prime => False));
         end loop;
         --  notify the threads that we are complete,
         --  so all of them are joined after
         --  the rendevouz is done
         for item in Thread_Index loop
            task_pool (item).PRODUCER_DONE;
         end loop;
      end;

      --  get the array for printing.
      result_array := Result_Array_Access.Get_Array;
      GNATCOLL.Traces.Trace(Log, "Task Completed!");
      return result_array;

   end calculate_spiral;
end Main;
