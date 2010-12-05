
with AUnit.Test_Cases, AUnit.Assertions;
use AUnit.Test_Cases, AUnit.Assertions;

with Memcache.Messages;
use Memcache.Messages;

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
    --  Tests
    --

    procedure Test_Stats(T: in out Test_Case'Class) is
        Stats_Command : Message(C => Stats);
    begin
        Assert(Serialize(Stats_Command) = "stats\r\n", "Serialized stats command is incorrect");
        null;
    end Test_Stats;

end Test_Messages;
