with Memcache;

with Test_Messages;

package body Suite is
    use AUnit.Test_Suites;
    function Suite return Access_Test_Suite is
        Result : constant Access_Test_Suite := new Test_Suite;
    begin
        Result.Add_Test(new Test_Messages.Messages_Test);
        --Result.Add_Test(new SimpleTest.Test);
        return Result;
    end Suite;
end Suite;
