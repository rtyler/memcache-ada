
with AUnit.Test_Cases, AUnit.Assertions;
use AUnit.Test_Cases, AUnit.Assertions;

package body Memcache.Client.Test.Decr is
    procedure Register_Tests (T : in out Decr_Test) is
        use AUnit.Test_Cases.Registration;
    begin
        Register_Routine (T, Test_Gen_Decr'Access,
                "Validate an decr call generates the " &
                "right string");
        Register_Routine (T, Test_Gen_Decr_No_Reply'Access,
                "Validate a noreply decr call generates the " &
                "right string");
        Register_Routine (T, Test_Gen_Decr_Bad_Key'Access,
                "Validate an decr call with a bad key raises");
    end Register_Tests;


    function Name (T : Decr_Test) return AUnit.Message_String is
        pragma Unreferenced (T);
    begin
        return AUnit.Format ("Test `Memcache.Decr` operations");
    end Name;


    procedure Test_Gen_Decr (T :
                      in out AUnit.Test_Cases.Test_Case'Class) is
        Command : String := Memcache.Client.Generate_Decr ("GoodKey", 100, False);
        Expected : String := Append_CRLF ("decr GoodKey 100");
    begin
        Assert (Command = Expected, "Bad command string");
    end Test_Gen_Decr;


    procedure Test_Gen_Decr_No_Reply (T :
                      in out AUnit.Test_Cases.Test_Case'Class) is
        Command : String := Memcache.Client.Generate_Decr ("GoodKey", 100, True);
        Expected : String := Append_CRLF ("decr GoodKey 100 noreply");
    begin
        Assert (Command = Expected, "Bad command string");
    end Test_Gen_Decr_No_Reply;


    procedure Test_Gen_Decr_Bad_Key (T :
                      in out AUnit.Test_Cases.Test_Case'Class) is
    begin
        declare
            Command : String := Memcache.Client.Generate_Decr ("Bad Key", 0, False);
        begin
            Assert (False, "Should have raised an Invalid_Key_Error");
        end;
    exception
        when Invalid_Key_Error =>
            Assert (True, "Properly raised Invalid_Key_Error");
    end Test_Gen_Decr_Bad_Key;
end Memcache.Client.Test.Decr;
