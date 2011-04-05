
with AUnit.Test_Cases, AUnit.Assertions;
use AUnit.Test_Cases, AUnit.Assertions;

package body Memcache.Client.Test.Get is
    procedure Register_Tests (T : in out Get_Test) is
        use AUnit.Test_Cases.Registration;
    begin
        Register_Routine (T, Test_Gen_Get'Access,
                "Validate an get call generates the " &
                "right string");
        Register_Routine (T, Test_Gen_Get_Bad_Key'Access,
                "Validate an get call with a bad key raises");
    end Register_Tests;


    function Name (T : Get_Test) return AUnit.Message_String is
        pragma Unreferenced (T);
    begin
        return AUnit.Format ("Test `Memcache.Get` operations");
    end Name;


    procedure Test_Gen_Get (T :
                      in out AUnit.Test_Cases.Test_Case'Class) is
        Command : String := Memcache.Client.Generate_Get ("GoodKey");
        Expected : String := Append_CRLF ("get GoodKey");
    begin
        Assert (Command = Expected, "Bad command string");
    end Test_Gen_Get;


    procedure Test_Gen_Get_Bad_Key (T :
                      in out AUnit.Test_Cases.Test_Case'Class) is
    begin
        declare
            Command : String := Memcache.Client.Generate_Get ("Bad Key");
        begin
            Assert (False, "Should have raised an Invalid_Key_Error");
        end;
    exception
        when Invalid_Key_Error =>
            Assert (True, "Properly raised Invalid_Key_Error");
    end Test_Gen_Get_Bad_Key;
end Memcache.Client.Test.Get;
