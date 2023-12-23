with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Containers.Synchronized_Queue_Interfaces;

with Ada.Strings.Unbounded;

with GNATCOLL.Traces;

generic
   array_size : in Positive;

package Main is

   Log : constant GNATCOLL.Traces.Trace_Handle := GNATCOLL.Traces.Create ("LOG", Stream => "&2", Default => GNATCOLL.Traces.On);


   type Ulam_Result is record
      number     : Positive;
      x_position : Natural;
      y_position : Natural;
      is_prime   : Boolean;
   end record;

   subtype Array_Index is Natural range 0 .. array_size - 1;

   type Ulam_Spiral is array (Array_Index, Array_Index) of Ulam_Result;

   type Ulam_Spiral_Access is access all Ulam_Spiral;

   function calculate_spiral return Ulam_Spiral_Access;

   function Join_Array (To_Join : Ulam_Spiral_Access) return Ada.Strings.Unbounded.Unbounded_String;

private

   subtype Process_Numbers is Positive range 1 .. array_size**2;
   subtype Thread_Index is Positive range 1 .. 5;

   --   Tasks Types
   task type Ulam_Task_Queue is
      entry PRODUCER_DONE;
   end Ulam_Task_Queue;

   --  Protected Objects declaration
   protected Result_Array_Access is
      procedure Set_Element (result : Ulam_Result);
      function Get_Array return Ulam_Spiral_Access;
   private
      result_array : Ulam_Spiral_Access := new Ulam_Spiral;
   end Result_Array_Access;

   --  Package instantiations
   package Data_Interface is new Ada.Containers.Synchronized_Queue_Interfaces
     (Element_Type => Ulam_Result);

   package Data_Queue is new Ada.Containers.Unbounded_Synchronized_Queues
     (Queue_Interfaces => Data_Interface);


end Main;
