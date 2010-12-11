
with AUnit.Test_Cases, AUnit.Assertions;
use AUnit.Test_Cases, AUnit.Assertions;

package body Memcache.Test is
    procedure Register_Tests (T : in out Client_Test) is
        use AUnit.Test_Cases.Registration;
    begin
        Register_Routine (T, Test_Validate_Empty_Key'Access,
                        "Validate that an empty key is erroneous");
    end Register_Tests;

    function Name (T : Client_Test) return AUnit.Message_String is
        pragma Unreferenced (T);
    begin
        return AUnit.Format ("Test `Memcache` operations");
    end Name;

    procedure Test_Validate_Empty_Key (T :
                    in out AUnit.Test_Cases.Test_Case'Class) is
    begin
        Memcache.Validate ("");
        Assert (False, "Should have raised an Invalid_Key_Error");
    exception
        when Invalid_Key_Error =>
            Assert (True, "Properly raised Invalid_Key_Error");
    end Test_Validate_Empty_Key;
end Memcache.Test;
