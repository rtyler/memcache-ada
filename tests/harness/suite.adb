with Memcache;
with Memcache.Client.Test;
with Memcache.Client.Test.Delete;
with Memcache.Client.Test.Incr;
with Memcache.Client.Test.Decr;
with Memcache.Client.Test.Store;
with Memcache.Client.Test.Get;

package body Suite is
    use AUnit.Test_Suites;
    function Suite return Access_Test_Suite is
        Result : constant Access_Test_Suite := new Test_Suite;
    begin
        Result.Add_Test (new Memcache.Client.Test.Client_Test);
        Result.Add_Test (new Memcache.Client.Test.Delete.Delete_Test);
        Result.Add_Test (new Memcache.Client.Test.Incr.Incr_Test);
        Result.Add_Test (new Memcache.Client.Test.Decr.Decr_Test);
        Result.Add_Test (new Memcache.Client.Test.Store.Store_Test);
        Result.Add_Test (new Memcache.Client.Test.Get.Get_Test);
        return Result;
    end Suite;
end Suite;
