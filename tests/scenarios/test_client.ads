
with AUnit;
with AUnit.Test_Cases;

package Test_Client is
    type Client_Test is new AUnit.Test_Cases.Test_Case with null record;

    procedure Register_Tests (T : in out Client_Test);
    function Name (T : Client_Test) return AUnit.Message_String;

end Test_Client;
