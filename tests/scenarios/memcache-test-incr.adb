
with AUnit.Test_Cases, AUnit.Assertions;
use AUnit.Test_Cases, AUnit.Assertions;

package body Memcache.Test.Incr is
    procedure Register_Tests (T : in out Incr_Test) is
        use AUnit.Test_Cases.Registration;
    begin
        null;
    end Register_Tests;


    function Name (T : Incr_Test) return AUnit.Message_String is
        pragma Unreferenced (T);
    begin
        return AUnit.Format ("Test `Memcache.Incr` operations");
    end Name;


    procedure Test_Gen_Incr (T :
                      in out AUnit.Test_Cases.Test_Case'Class) is
        Command : String := Memcache.Generate_Incr ("GoodKey", 0, False);
        Expected : String := Append_CRLF ("incr GoodKey 0");
    begin
        Assert (Command = Expected, "Bad command string");
    end Test_Gen_Incr;
end Memcache.Test.Incr;
