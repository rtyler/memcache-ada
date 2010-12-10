
with AUnit.Test_Cases, AUnit.Assertions;
use AUnit.Test_Cases, AUnit.Assertions;

package body Test_Client is
    procedure Register_Tests (T : in out Client_Test) is
        use AUnit.Test_Cases.Registration;
    begin
        null;
    end Register_Tests;

    function Name (T : Client_Test) return AUnit.Message_String is
        pragma Unreferenced (T);
    begin
        return AUnit.Format ("Test `Memcache` operations");
    end Name;
end Test_Client;
