
with AUnit;
with AUnit.Test_Cases;

package Memcache.Test.Get is
    type Get_Test is new AUnit.Test_Cases.Test_Case with null record;

    procedure Register_Tests (T : in out Get_Test);
    function Name (T : Get_Test) return AUnit.Message_String;

    procedure Test_Gen_Get (T :
                      in out AUnit.Test_Cases.Test_Case'Class);
    procedure Test_Gen_Get_Bad_Key (T :
                      in out AUnit.Test_Cases.Test_Case'Class);
end Memcache.Test.Get;


