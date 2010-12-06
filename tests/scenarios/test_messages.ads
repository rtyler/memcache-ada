
with AUnit;
with AUnit.Test_Cases;

package Test_Messages is
    type Messages_Test is new AUnit.Test_Cases.Test_Case with null record;

    procedure Register_Tests (T: in out Messages_Test);
    function Name (T: Messages_Test) return AUnit.Message_String;

    procedure Test_Stats(T: in out AUnit.Test_Cases.Test_Case'Class);
    procedure Test_Get_Multiple_Keys(T: in out AUnit.Test_Cases.Test_Case'Class);
    procedure Test_Get_Single_Key(T: in out AUnit.Test_Cases.Test_Case'Class);
    procedure Test_Get_No_Key(T: in out AUnit.Test_Cases.Test_Case'Class);
end Test_Messages;
