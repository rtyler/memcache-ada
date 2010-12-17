
with AUnit;
with AUnit.Test_Cases;

package Memcache.Test.Incr is
    type Incr_Test is new AUnit.Test_Cases.Test_Case with null record;

    procedure Register_Tests (T : in out Incr_Test);
    function Name (T : Incr_Test) return AUnit.Message_String;

    procedure Test_Gen_Incr (T :
                      in out AUnit.Test_Cases.Test_Case'Class);
    procedure Test_Gen_Incr_No_Reply (T :
                      in out AUnit.Test_Cases.Test_Case'Class);
    procedure Test_Gen_Incr_Bad_Key (T :
                      in out AUnit.Test_Cases.Test_Case'Class);
end Memcache.Test.Incr;

