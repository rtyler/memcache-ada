--
--  Primary package specification for the Ada memcached client library
--

package Memcache is
    type Client is tagged record
        Port : Natural := 11211;
        Host : String (1 .. 256);
    end record;

    procedure Connect(M : in out Client; Host : in String; Port : in Natural);
end Memcache;
