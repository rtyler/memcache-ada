
with AUnit.Test_Cases, AUnit.Assertions;
use AUnit.Test_Cases, AUnit.Assertions;

with Memcache.Messages;
use Memcache.Messages;

package body Test_Messages is

    procedure Register_Tests(T: in out Messages_Test) is
        use AUnit.Test_Cases.Registration;
    begin
        Register_Routine(T, Test_Stats'Access, "Stats Message");

        Register_Routine(T, Test_Get_Single_Key'Access, "Get a single key");
        Register_Routine(T, Test_Get_Multiple_Keys'Access, "Get multiple keys");
        Register_Routine(T, Test_Get_Multiple_Bad_Keys'Access, "Get multiple bad keys (error)");
        Register_Routine(T, Test_Get_No_Key'Access, "Get with no key (error)");
        Register_Routine(T, Test_Get_Space_Key'Access, "Get with a key with a space (error)");
        Register_Routine(T, Test_Get_Space_Key2'Access, "Get with a key with a space #2 (error)");

        Register_Routine(T, Test_Delete_Key'Access, "Delete a key");
        Register_Routine(T, Test_Delete_No_Key'Access, "Delete with no key (error)");
        Register_Routine(T, Test_Delete_Space_Key'Access, "Delete with a key with a space (error)");
    end Register_Tests;


    function Name(T : Messages_Test) return AUnit.Message_String is
        pragma Unreferenced(T);
    begin
        return AUnit.Format("Test Memcache.Messages");
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
        Keys : Key_Vectors.Vector;
        Get_Command : Get;
    begin
        Keys.Append(Bounded.To_Bounded_String("One"));
        Keys.Append(Bounded.To_Bounded_String("Two"));
        Get_Command := Create(Keys);
        Assert(Serialize(Get_Command) = "get One Two\r\n", "Serialized get command with multiple keys is incorrect");
    end Test_Get_Multiple_Keys;


    procedure Test_Get_Multiple_Bad_Keys(T: in out AUnit.Test_Cases.Test_Case'Class) is
        Get_Command : Get;
        Keys : Key_Vectors.Vector;
    begin
        Keys.Append(Bounded.To_Bounded_String("Good Key"));
        Keys.Append(Bounded.To_Bounded_String("Bad Key"));
        Get_Command.Keys := Keys;
        declare
            Dummy : String := Serialize(Get_Command);
        begin
            null;
        end;
        Assert(False, "Should have raised an Invalid_Key_Error");
    exception
        when Invalid_Key_Error =>
            Assert(True, "Properly raised Invalid_Key_Error");
        when others =>
            Assert(False, "Raised the wrong exception");
    end Test_Get_Multiple_Bad_Keys;


    procedure Test_Get_No_Key(T: in out Test_Case'Class) is
        Get_Command : Get;
    begin
        Get_Command := Create("");
        Assert(False, "Should have raised an exception here");
    exception
        when Invalid_Key_Error =>
            Assert(True, "Properly raised Invalid_Key_Error");
        when others =>
            Assert(False, "Raised the wrong exception");
    end Test_Get_No_Key;


    procedure Test_Get_Space_Key(T: in out Test_Case'Class) is
        Get_Command : Get;
    begin
        Get_Command := Create("Bad Key");
        Assert(False, "Should have raised an exception here");
    exception
        when Invalid_Key_Error =>
            Assert(True, "Properly raised Invalid_Key_Error");
        when others =>
            Assert(False, "Raised the wrong exception");
    end Test_Get_Space_Key;


    --  Create a Get message with no Key, then try to serialize it after
    --  setting a erroneous key after the fact
    procedure Test_Get_Space_Key2(T: in out AUnit.Test_Cases.Test_Case'Class) is
        Get_Command : Get;
        Keys : Key_Vectors.Vector;
    begin
        Keys.Append(Bounded.To_Bounded_String("Test Key"));
        Get_Command.Keys := Keys;
        declare
            Dummy : String := Serialize(Get_Command);
        begin
            null;
        end;
        Assert(False, "Should have raised an Invalid_Key_Error");
    exception
        when Invalid_Key_Error =>
            Assert(True, "Properly raised Invalid_Key_Error");
        when others =>
            Assert(False, "Raised the wrong exception");
    end Test_Get_Space_Key2;


    procedure Test_Delete_Key(T: in out AUnit.Test_Cases.Test_Case'Class) is
        Delete_Command : Delete := Create("Test_Key");
    begin
        Assert(Serialize(Delete_Command) = "delete Test_Key\r\n", "Incorrect delete command");
    end Test_Delete_Key;


    procedure Test_Delete_No_Key(T: in out AUnit.Test_Cases.Test_Case'Class) is
        Delete_Command : Delete;
    begin
        Assert(False, "not implemented");
    end Test_Delete_No_Key;


    procedure Test_Delete_Space_Key(T: in out AUnit.Test_Cases.Test_Case'Class) is
        Delete_Command : Delete := Create("Test Key");
    begin
        Assert(False, "not implemented");
    end Test_Delete_Space_Key;

end Test_Messages;
