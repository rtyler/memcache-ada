
with AUnit;
with AUnit.Test_Cases;

package Memcache.Test.Set is
    type Set_Test is new AUnit.Test_Cases.Test_Case with null record;

    procedure Register_Tests (T : in out Set_Test);
    function Name (T : Set_Test) return AUnit.Message_String;

    procedure Test_Gen_Set (T :
                      in out AUnit.Test_Cases.Test_Case'Class);
    procedure Test_Gen_Set_Calendar (T :
                      in out AUnit.Test_Cases.Test_Case'Class);
end Memcache.Test.Set;

