

with AUnit.Test_Cases, AUnit.Assertions;
use AUnit.Test_Cases, AUnit.Assertions;

package body Test_Client_Delete is
    procedure Register_Tests (T : in out Delete_Test) is
        use AUnit.Test_Cases.Registration;
    begin
        null;
    end Register_Tests;

    function Name (T : Delete_Test) return AUnit.Message_String is
        pragma Unreferenced (T);
    begin
        return AUnit.Format ("Test `Memcache.Delete` operations");
    end Name;
end Test_Client_Delete;
