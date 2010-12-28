with Memcache;
with Memcache.Test;
with Memcache.Test.Delete;
with Memcache.Test.Incr;
with Memcache.Test.Decr;
with Memcache.Test.Store;
with Memcache.Test.Get;

package body Suite is
    use AUnit.Test_Suites;
    function Suite return Access_Test_Suite is
        Result : constant Access_Test_Suite := new Test_Suite;
    begin
        Result.Add_Test (new Memcache.Test.Client_Test);
        Result.Add_Test (new Memcache.Test.Delete.Delete_Test);
        Result.Add_Test (new Memcache.Test.Incr.Incr_Test);
        Result.Add_Test (new Memcache.Test.Decr.Decr_Test);
        Result.Add_Test (new Memcache.Test.Store.Store_Test);
        Result.Add_Test (new Memcache.Test.Get.Get_Test);
        return Result;
    end Suite;
end Suite;
