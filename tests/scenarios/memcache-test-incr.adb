
with AUnit.Test_Cases, AUnit.Assertions;
use AUnit.Test_Cases, AUnit.Assertions;
with Ada.Text_IO;

package body Memcache.Test.Incr is
    procedure Register_Tests (T : in out Incr_Test) is
        use AUnit.Test_Cases.Registration;
    begin
        Register_Routine (T, Test_Gen_Incr'Access,
                "Validate an incr call generates the " &
                "right string");
        Register_Routine (T, Test_Gen_Incr_No_Reply'Access,
                "Validate a noreply incr call generates the " &
                "right string");
        Register_Routine (T, Test_Gen_Incr_Bad_Key'Access,
                "Validate an incr call with a bad key raises");
    end Register_Tests;


    function Name (T : Incr_Test) return AUnit.Message_String is
        pragma Unreferenced (T);
    begin
        return AUnit.Format ("Test `Memcache.Incr` operations");
    end Name;


    procedure Test_Gen_Incr (T :
                      in out AUnit.Test_Cases.Test_Case'Class) is
        Command : String := Memcache.Generate_Incr ("GoodKey", 100, False);
        Expected : String := Append_CRLF ("incr GoodKey 100");
    begin
        Assert (Command = Expected, "Bad command string");
    end Test_Gen_Incr;


    procedure Test_Gen_Incr_No_Reply (T :
                      in out AUnit.Test_Cases.Test_Case'Class) is
        Command : String := Memcache.Generate_Incr ("GoodKey", 100, True);
        Expected : String := Append_CRLF ("incr GoodKey 100 noreply");
    begin
        Assert (Command = Expected, "Bad command string");
    end Test_Gen_Incr_No_Reply;


    procedure Test_Gen_Incr_Bad_Key (T :
                      in out AUnit.Test_Cases.Test_Case'Class) is
    begin
        declare
            Command : String := Memcache.Generate_Incr ("Bad Key", 0, False);
        begin
            Assert (False, "Should have raised an Invalid_Key_Error");
        end;
    exception
        when Invalid_Key_Error =>
            Assert (True, "Properly raised Invalid_Key_Error");
    end Test_Gen_Incr_Bad_Key;
end Memcache.Test.Incr;
