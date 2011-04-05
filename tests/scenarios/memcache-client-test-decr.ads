
with AUnit;
with AUnit.Test_Cases;

package Memcache.Client.Test.Decr is
    type Decr_Test is new AUnit.Test_Cases.Test_Case with null record;

    procedure Register_Tests (T : in out Decr_Test);
    function Name (T : Decr_Test) return AUnit.Message_String;

    procedure Test_Gen_Decr (T :
                      in out AUnit.Test_Cases.Test_Case'Class);
    procedure Test_Gen_Decr_No_Reply (T :
                      in out AUnit.Test_Cases.Test_Case'Class);
    procedure Test_Gen_Decr_Bad_Key (T :
                      in out AUnit.Test_Cases.Test_Case'Class);
end Memcache.Client.Test.Decr;

