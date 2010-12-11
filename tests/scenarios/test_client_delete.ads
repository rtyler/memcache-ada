--
--  AUnit tests for testing all Memcache.Delete calls
--


with AUnit;
with AUnit.Test_Cases;

package Test_Client_Delete is
    type Delete_Test is new AUnit.Test_Cases.Test_Case with null record;

    procedure Register_Tests (T : in out Delete_Test);
    function Name (T : Delete_Test) return AUnit.Message_String;

end Test_Client_Delete;
