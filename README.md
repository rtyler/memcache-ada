memcache-ada
============

**memcache-ada** is a work in progress, implementing the Memcached text-protocol
for communicating with a Memcached server from an Ada program.

Status
------

**memcache-ada** is currently in a very usable state for basic cache usage, that
said, the library is not very well documented just yet. The source code in the `client/`
directory is fairly straight-forward, so I suggest consulting that if you have any
questions

#### Implemented Calls

    +----------------------------------------------------------+
    |    Name      |  Status  |           Notes                |
    +----------------------------------------------------------+
    | get          |  y       |                                |
    | gets         |  n       |                                |
    +--------------+-------------------------------------------+
    | set          |  y       |                                |
    | add          |  y       |                                |
    | replace      |  y       |                                |
    | append       |  y       |                                |
    | prepend      |  y       |                                |
    | cas          |  n       |                                |
    +--------------+-------------------------------------------+
    | delete       |  y       |                                |
    +--------------+-------------------------------------------+
    | incr         |  y       |                                |
    | decr         |  y       |                                |
    +--------------+-------------------------------------------+
    | stats        |  y       |                                |
    | stats (args) |  n       |                                |
    | flush_all    |  y*      | delayed flush not supported    |
    | version      |  y       |                                |
    | verbosity    |  n       | no plans to support            |
    | quit         |  n       | no plans to support            |
    +--------------+-------------------------------------------+

Warnings
--------

The `Memcache.Connection` object ***cannot*** be shared between multiple tasks
concurrently! It is currently designed only for use within a single task at a
time and offers *zero protection* between tasks. It is better to use multiple
objects across multiple tasks if necessary (I have some ideas regarding connection
pooling and sharing between tasks but the library is not quite there yet).


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
        package SU renames Ada.Strings.Unbounded;
        C : Memcache.Connection := Memcache.Create (Host => "127.0.0.1",
                                        Port => 11211);
    begin
        C.Connect;
        declare
            Info : Memcache.Response := C.Get ("SomeKey");
        begin
            Ada.Text_IO.Put_Line ("`SomeKey` => " & SU.To_String(Info.Data));
        end;
        C.Disconnect;
    end Sample;
