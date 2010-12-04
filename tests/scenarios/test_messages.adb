
with AUnit.Assertions;
use AUnit.Assertions;

package body Test_Messages is

    procedure Register_Tests(T: in out Messages_Test) is
        use AUnit.Test_Cases.Registration;
    begin
        Register_Routine(T, Test_Stats'Access, "Test Stats Message");
    end Register_Tests;


    function Name(T : Messages_Test) return AUnit.Message_String is
        pragma Unreferenced(T);
    begin
        return AUnit.Format("Test_Messages");
    end Name;


    --
    --  Test methods
    --
    procedure Test_Stats(T: in out AUnit.Test_Cases.Test_Case'Class) is
    begin
        Assert(False, "not implemented");
        null;
    end Test_Stats;

end Test_Messages;
