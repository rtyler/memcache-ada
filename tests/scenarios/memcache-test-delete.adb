
with AUnit.Test_Cases, AUnit.Assertions;
use AUnit.Test_Cases, AUnit.Assertions;

package body Memcache.Test.Delete is
    procedure Register_Tests (T : in out Delete_Test) is
        use AUnit.Test_Cases.Registration;
    begin
        Register_Routine (T, Test_Gen_Delete'Access,
                "Validate a delete call generates the right string");
        Register_Routine (T, Test_Gen_Delete_Delayed'Access,
                "Validate a delayed delete call generates the right string");
        Register_Routine (T, Test_Gen_Delete_No_Reply'Access,
                "Validate a no-reply delete call generates the right string");
        Register_Routine (T, Test_Gen_Delete_Delayed_No_Reply'Access,
                "Validate a no-reply delayed delete call generates " &
                "the right string");
    end Register_Tests;


    function Name (T : Delete_Test) return AUnit.Message_String is
        pragma Unreferenced (T);
    begin
        return AUnit.Format ("Test `Memcache.Delete` operations");
    end Name;

    procedure Test_Gen_Delete (T :
                      in out AUnit.Test_Cases.Test_Case'Class) is
        Command : String := Memcache.Generate_Delete ("GoodKey", 0, False);
    begin
        Assert (Command = "delete GoodKey\r\n", "Bad command string");
    end Test_Gen_Delete;


    procedure Test_Gen_Delete_Delayed (T :
                      in out AUnit.Test_Cases.Test_Case'Class) is
        Command : String := Memcache.Generate_Delete ("GoodKey", 10, False);
    begin
        Assert (Command = "delete GoodKey 10\r\n", "Bad command string");
    end Test_Gen_Delete_Delayed;


    procedure Test_Gen_Delete_No_Reply (T :
                      in out AUnit.Test_Cases.Test_Case'Class) is
        Command : String := Memcache.Generate_Delete ("GoodKey", 0, True);
    begin
        Assert (Command = "delete GoodKey noreply\r\n", "Bad command string");
    end Test_Gen_Delete_No_Reply;


    procedure Test_Gen_Delete_Delayed_No_Reply (T :
                      in out AUnit.Test_Cases.Test_Case'Class) is
        Command : String := Memcache.Generate_Delete ("GoodKey", 10, True);
    begin
        Assert (Command = "delete GoodKey 10 noreply\r\n",
            "Bad command string");
    end Test_Gen_Delete_Delayed_No_Reply;
end Memcache.Test.Delete;
