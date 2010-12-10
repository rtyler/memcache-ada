--
--  memcache-ada test runner
--

with AUnit.Run;
with AUnit.Reporter.Text;
use AUnit.Reporter.Text;

with Suite;

procedure TestRunner is
    procedure Runner is new AUnit.Run.Test_Runner (Suite.Suite);
    Reporter : Text_Reporter;
begin
    Set_Use_ANSI_Colors (Engine => Reporter,
            Value => True);
    Runner (Reporter);
end TestRunner;
