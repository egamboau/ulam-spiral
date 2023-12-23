separate (main)

protected body Result_Array_Access is
   procedure Set_Element (result : Ulam_Result) is
   begin
      result_array (result.y_position , result.x_position) := result;
   end Set_Element;

   function Get_Array return Ulam_Spiral_Access is
   begin
      return result_array;
   end Get_Array;

end Result_Array_Access;
