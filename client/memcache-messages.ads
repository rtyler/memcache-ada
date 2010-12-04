--
--  Memcache.Messages specification
--
--  This spec should define all the supported messages we can send to
--  the memcached server running.
--

package Memcache.Messages is
    type BasicMessage is tagged record
        Name : String (1 .. 32);
    end record;
end Memcache.Messages;
