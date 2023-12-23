with AUnit.Reporter.Text;
with AUnit.Run;

with Test_Container_Suite;

procedure Tests is
   procedure Runner is new AUnit.Run.Test_Runner (Test_Container_Suite.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   AUnit.Reporter.Text.Set_Use_ANSI_Colors (Reporter, True);
   Runner (Reporter);
end Tests;
