

with AUnit;
with AUnit.Test_Cases;

package Memcache.Test.Delete is
    type Delete_Test is new AUnit.Test_Cases.Test_Case with null record;

    procedure Register_Tests (T : in out Delete_Test);
    function Name (T : Delete_Test) return AUnit.Message_String;


    procedure Test_Gen_Delete (T :
                      in out AUnit.Test_Cases.Test_Case'Class);
    procedure Test_Gen_Delete_Delayed (T :
                      in out AUnit.Test_Cases.Test_Case'Class);
    procedure Test_Gen_Delete_No_Reply (T :
                      in out AUnit.Test_Cases.Test_Case'Class);
    procedure Test_Gen_Delete_Delayed_No_Reply (T :
                      in out AUnit.Test_Cases.Test_Case'Class);
end Memcache.Test.Delete;
