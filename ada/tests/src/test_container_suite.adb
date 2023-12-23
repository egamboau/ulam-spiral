with AUnit.Test_Caller;

with Main_Package_Tests;

package body Test_Container_Suite is

   package Caller is new AUnit.Test_Caller (Main_Package_Tests.Test);

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Ret : constant AUnit.Test_Suites.Access_Test_Suite := new AUnit.Test_Suites.Test_Suite;
   begin
      Ret.Add_Test(Caller.Create("Array Join Test", Main_Package_Tests.Test_Join_Array'Access));
      Ret.Add_Test(Caller.Create("Generate 6 by 6 spiral", Main_Package_Tests.Generate_6_by_6_spiral'Access));
      return Ret;
   end Suite;

end Test_Container_Suite;
