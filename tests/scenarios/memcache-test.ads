
with AUnit;
with AUnit.Test_Cases;

package Memcache.Test is
    type Client_Test is new AUnit.Test_Cases.Test_Case with null record;

    procedure Register_Tests (T : in out Client_Test);
    function Name (T : Client_Test) return AUnit.Message_String;


    procedure Test_Validate_Empty_Key (T :
                    in out AUnit.Test_Cases.Test_Case'Class);
    procedure Test_Validate_Long_Key (T :
                    in out AUnit.Test_Cases.Test_Case'Class);
    procedure Test_Validate_Space_Key (T :
                    in out AUnit.Test_Cases.Test_Case'Class);
    procedure Test_Validate_Space_End_Key (T :
                    in out AUnit.Test_Cases.Test_Case'Class);
    procedure Test_Validate_Tab_Key (T :
                    in out AUnit.Test_Cases.Test_Case'Class);
    procedure Test_Validate_Newline_Key (T :
                    in out AUnit.Test_Cases.Test_Case'Class);

end Memcache.Test;
