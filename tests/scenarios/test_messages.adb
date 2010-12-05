
with AUnit.Test_Cases, AUnit.Assertions;
use AUnit.Test_Cases, AUnit.Assertions;

with Memcache.Messages;
use Memcache.Messages;

package body Test_Messages is

    procedure Register_Tests(T: in out Messages_Test) is
        use AUnit.Test_Cases.Registration;
    begin
        Register_Routine(T, Test_Stats'Access, "Test Stats Message");
        Register_Routine(T, Test_Get_Single_Key'Access, "Test GETing a single key");
        Register_Routine(T, Test_Get_Multiple_Keys'Access, "Test GETing multiple keys");
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
        Stats_Command : Stats := Create;
    begin
        Assert(Serialize(Stats_Command) = "stats\r\n", "Serialized stats command is incorrect");
        null;
    end Test_Stats;

    procedure Test_Get_Single_Key(T: in out Test_Case'Class) is
        Get_Command : Get := Create("Test_Key");
    begin
        Assert(Serialize(Get_Command) = "get Test_Key\r\n", "Serialized get command is incorrect");
    end Test_Get_Single_Key;

    procedure Test_Get_Multiple_Keys(T: in out Test_Case'Class) is
    begin
        null;
    end Test_Get_Multiple_Keys;

end Test_Messages;
