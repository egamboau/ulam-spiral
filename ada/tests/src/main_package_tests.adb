with Main;
with Ada.Strings.Unbounded;

with AUnit.Assertions;

package body Main_Package_Tests is

   procedure Set_Up (T : in out Test) is
   begin
      null;
   end Set_Up;

   procedure Test_Join_Array (T : in out Test) is
      package Ulam_Entry_Point is new Main (array_size => 2);
      use Ulam_Entry_Point;
      test_result    : Ada.Strings.Unbounded.Unbounded_String;
      expected_resul : constant String := "--" & Character'Val (10) & "--";
      result_array   : constant Ulam_Spiral_Access := new Ulam_Spiral;
   begin
      result_array (0, 0) :=
        (number => 1, x_position => 1, y_position => 1, is_prime => False);
      result_array (0, 1) :=
        (number => 2, x_position => 1, y_position => 1, is_prime => False);
      result_array (1, 0) :=
        (number => 3, x_position => 1, y_position => 1, is_prime => False);
      result_array (1, 1) :=
        (number => 4, x_position => 1, y_position => 1, is_prime => False);
      test_result         := Ulam_Entry_Point.Join_Array (result_array);
      AUnit.Assertions.Assert
        (Ada.Strings.Unbounded.To_String (Source => test_result),
         expected_resul, "Incorrect Joined Array");
   end Test_Join_Array;

   procedure Generate_6_by_6_spiral (T : in out Test) is
      package Ulam_Entry_Point is new Main (array_size => 6);
      use Ulam_Entry_Point;
      spiral         : Ulam_Spiral_Access;
      checkElement   : Ulam_Result;
      expectedResult : constant String :=
        "-----*" & Character'Val (10) & "*---*-" & Character'Val (10) &
        "-*-*-*" & Character'Val (10) & "*--**-" & Character'Val (10) &
        "-*----" & Character'Val (10) & "--*---";
   begin
      spiral := Ulam_Entry_Point.calculate_spiral;

      --  some spotchecks. Element 2,3 must be 1
      --  as usual, the array first need to go to Y, then X
      checkElement := spiral (3, 2);
      AUnit.Assertions.Assert
        (checkElement.number = 1,
         "Number 1 is not correctly placed. Got " & checkElement'Image);

      --  Now for position 33
      checkElement := spiral (0, 3);
      AUnit.Assertions.Assert
        (checkElement.number = 33,
         "Number 33 is not correctly placed. Got " & checkElement'Image);

      --  And now, 24
      checkElement := spiral (5, 3);
      AUnit.Assertions.Assert
        (checkElement.number = 24,
         "Number 24 is not correctly placed. Got " & checkElement'Image);

      AUnit.Assertions.Assert
        (Ada.Strings.Unbounded.To_String
           (Source => Ulam_Entry_Point.Join_Array (spiral)),
         expectedResult, "Incorrect Joined Array");
   end Generate_6_by_6_spiral;
end Main_Package_Tests;
