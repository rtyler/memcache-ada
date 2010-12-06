--
--  memcache-ada xml test runner
--

with AUnit.Run;
with AUnit.Reporter.Xml;
use AUnit.Reporter.Xml;

with Suite;

procedure XmlRunner is
    procedure Runner is new AUnit.Run.Test_Runner(Suite.Suite);
    Reporter : Xml_Reporter;
begin
    Runner(Reporter);
end XmlRunner;
