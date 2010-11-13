--
--  memcache-ada test runner
--

with AUnit.Run;
with AUnit.Reporter.Text;

with Suite;

procedure TestRunner is
    procedure Runner is new AUnit.Run.Test_Runner(Suite.Suite);
    Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
    Runner(Reporter);
end TestRunner;
