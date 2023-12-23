with AUnit;
with AUnit.Test_Fixtures;



package Main_Package_Tests is

   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Set_Up (T : in out Test);

   procedure Test_Join_Array(T : in out Test);

   procedure Generate_6_by_6_spiral(T: in out Test);

end Main_Package_Tests;
