with Memcache;
with Memcache.Test;
with Memcache.Test.Delete;

package body Suite is
    use AUnit.Test_Suites;
    function Suite return Access_Test_Suite is
        Result : constant Access_Test_Suite := new Test_Suite;
    begin
        Result.Add_Test (new Memcache.Test.Client_Test);
        Result.Add_Test (new Memcache.Test.Delete.Delete_Test);
        return Result;
    end Suite;
end Suite;
