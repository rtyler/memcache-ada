memcache-ada
============

memcache-ada is an work in progress, implementing the Memcached text-protocol
for communicating with a Memcached server from an Ada program.


Examples
--------

**Creating a connection:**

    procedure Sample is
        C : Memcache.Connection := Memcache.Create (Host => "127.0.0.1",
                                        Port => 11211);
    begin
        C.Connect;
        -- Do stuff
        C.Disconnect;
    end Sample;


**Setting a value:**

    procedure Sample is
        C : Memcache.Connection := Memcache.Create (Host => "127.0.0.1",
                                        Port => 11211);
    begin
        C.Connect;
        C.Set ("SomeKey", "This sentence is so important, I need to cache it for five minutes", 300);
        C.Disconnect;
    end Sample;


**Getting a value:**

    procedure Sample is
        C : Memcache.Connection := Memcache.Create (Host => "127.0.0.1",
                                        Port => 11211);
    begin
        C.Connect;
        C.Get ("SomeKey");
        C.Disconnect;
    end Sample;
