
with AUnit;
with AUnit.Test_Cases;

package Memcache.Client.Test.Store is
    type Store_Test is new AUnit.Test_Cases.Test_Case with null record;

    procedure Register_Tests (T : in out Store_Test);
    function Name (T : Store_Test) return AUnit.Message_String;

    procedure Test_Gen_Set (T :
                      in out AUnit.Test_Cases.Test_Case'Class);
    procedure Test_Gen_Set_Calendar (T :
                      in out AUnit.Test_Cases.Test_Case'Class);
    procedure Test_Gen_Add (T :
                      in out AUnit.Test_Cases.Test_Case'Class);
    procedure Test_Gen_Replace (T :
                      in out AUnit.Test_Cases.Test_Case'Class);
    procedure Test_Gen_Prepend (T :
                      in out AUnit.Test_Cases.Test_Case'Class);
    procedure Test_Gen_Append (T :
                      in out AUnit.Test_Cases.Test_Case'Class);
end Memcache.Client.Test.Store;
