
with AUnit.Test_Cases, AUnit.Assertions;
use AUnit.Test_Cases, AUnit.Assertions;

package body Memcache.Test.Set is
    procedure Register_Tests (T : in out Set_Test) is
        use AUnit.Test_Cases.Registration;
    begin
        Register_Routine (T, Test_Gen_Set'Access,
                "Validate an set call generates the " &
                "right string");
    end Register_Tests;


    function Name (T : Set_Test) return AUnit.Message_String is
        pragma Unreferenced (T);
    begin
        return AUnit.Format ("Test `Memcache.Set` operations");
    end Name;


    procedure Test_Gen_Set (T :
                      in out AUnit.Test_Cases.Test_Case'Class) is
        Command : String := Generate_Set ("sets", "magicvalue", 0,
                                                60.0, False);
        Expected : String := "set sets 0 60 10" &
                            ASCII.CR & ASCII.LF &
                            "magicvalue" & ASCII.CR & ASCII.LF;
    begin
        Assert (Command = Expected, "Bad `set` command string");
    end Test_Gen_Set;
end Memcache.Test.Set;
