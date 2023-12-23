with Ada.Numerics.Elementary_Functions;

separate (Main)

task body Ulam_Task_Queue is
   use Ada.Containers;
   --   Package renames
   package EF renames Ada.Numerics.Elementary_Functions;

   --   Variables for the task
   Stop_Reading         : Boolean := False;
   Data_To_Process      : Ulam_Result;
   process_number       : Boolean := True;
   complete_squares     : Integer;
   x                    : Integer;
   y                    : Integer;
   distance_last_square : Integer;
   y_offset             : Integer := -1;
   x_offset             : Integer := -1;
   is_prime             : Boolean;
   current_value        : Integer;

   --   Functions for program
   function is_odd (number : Integer) return Boolean is
   begin
      return (number mod 2) = 1;
   end is_odd;

   function calculate_first_natural_numbers_alternate_series
     (n : Integer) return Integer
   is
   begin
      if is_odd (n) then
         return (n + 1) / 2;
      else
         return (-n) / 2;
      end if;
   end calculate_first_natural_numbers_alternate_series;
begin
   loop
      exit when Stop_Reading and then Current_Number_Queue.Current_Use = 0;
      select when not Stop_Reading =>
         accept PRODUCER_DONE do
            Stop_Reading := True;
         end PRODUCER_DONE;
      else
         --  try and read from the queue. Another tasks may already consumed
         --  the queue when the code reaches here, so a time out will suffice
         --  to recheck.
         select
            Current_Number_Queue.Dequeue (Element => Data_To_Process);
            process_number := True;
         or
            delay 1.0;
            process_number := False;
         end select;

         --  process the number, if we did not time out
         if process_number then
            if Data_To_Process.number = 1 then
               Data_To_Process.is_prime := False;
            else
               --  Check how many squares we had completed. ith this, we can
               --  check where the number should be by "walking" on the spiral
               complete_squares     :=
                 Integer
                   (Float'Truncation
                      (EF.Sqrt (X => Float (Data_To_Process.number))));
               --  get the position for the last completed square.
               --  x is the same if there are 1 or 2 complete squares
               --  however, y is maintained only if the complete square is 1
               --  so, we set y on a ternary op on that specific case.
               --  Oterwise, just use the calculation as is needed
               x                    := Data_To_Process.x_position;
               y                    :=
                 (if complete_squares = 2 then Data_To_Process.y_position - 1
                  else Data_To_Process.y_position);
               distance_last_square :=
                 Data_To_Process.number - (complete_squares**2);
               if complete_squares > 2 then
                  x :=
                    x +
                    calculate_first_natural_numbers_alternate_series
                      (complete_squares - 2);
                  y :=
                    y -
                    calculate_first_natural_numbers_alternate_series
                      (complete_squares - 1);
               end if;

               --  if the distance is 0, we already have x and y to return,
               --  using the previous calculations however, we need to "walk"
               --  over the spiral to cover any other gap
               if distance_last_square /= 0 then
                  --  the walk direction changes if the complete squares
                  --  is even, or odd.
                  y_offset := -1;
                  x_offset := -1;

                  if distance_last_square <= complete_squares then
                     x_offset := (-1)**(complete_squares + 1);
                     y_offset :=
                       ((distance_last_square * ((-1)**(complete_squares))) +
                        ((-1)**(complete_squares + 1)));
                  else
                     x_offset :=
                       ((-1)**(complete_squares + 1)) +
                       ((distance_last_square - complete_squares - 1) *
                        ((-1)**complete_squares));
                     y_offset :=
                       -(complete_squares * ((-1)**(complete_squares + 1)));
                  end if;
                  y := y + y_offset;
                  x := x + x_offset;
               end if;

               --  if the number is 2 or 3, the number is prime.
               --  Also, if the number not divisible by 2 or 3,
               --  the number is prime

               is_prime :=
                 (Data_To_Process.number <= 3)
                 or else
                 (Data_To_Process.number mod 2 > 0
                  and then Data_To_Process.number mod 3 > 0);

               if is_prime then
                  --  check with the formula 6k+-1.
                  --  the first value is (6*1) - 1, which is 5
                  --  also, (6*1) + 1, which is 7, or 5 + 2
                  current_value := 5;
                  while current_value <= complete_squares loop
                     --  CHECK CONDITION HERE!!!
                     if (Data_To_Process.number mod current_value = 0)
                       or else
                       (Data_To_Process.number mod (current_value + 2) = 0)
                     then
                        --   found a factor, the number is not prime
                        is_prime := False;
                     end if;
                     exit when is_prime = False;
                     --   Adding 1 to k is the same as adding 6 to
                     --   the current value.
                     current_value := current_value + 6;
                  end loop;
               end if;
               Data_To_Process.x_position := x;
               Data_To_Process.y_position := y;
               Data_To_Process.is_prime   := is_prime;
            end if;
            --   set the element to the array
            Result_Array_Access.Set_Element (Data_To_Process);
            GNATCOLL.Traces.Trace
              (Log,
               "Completed task for number" & Data_To_Process.number'Image &
               ", is prime: " & Data_To_Process.is_prime'Image);
         end if;

      end select;
   end loop;
end Ulam_Task_Queue;
