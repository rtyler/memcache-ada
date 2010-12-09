--
--  Primary Ada Memcached client specification
--
--  This package defines the core API accessible by
--  users of this library
--

package Memcache is
    type Connection is tagged private;

private

    type Connection is tagged record
        Port : Natural;
    end record;

end Memcache;
