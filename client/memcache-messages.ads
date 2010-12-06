--
--  Memcache.Messages specification
--
--  This spec should define all the supported messages we can send to --  the memcached server running.
--

with Ada.Strings.Bounded;
with Ada.Containers.Vectors;
use Ada.Containers;

package Memcache.Messages is
    package Bounded is new Ada.Strings.Bounded.Generic_Bounded_Length(Max => 250);
    use type Bounded.Bounded_String; -- Pull in operators for Bounded_String
    package Key_Vectors is new Vectors(Natural, Bounded.Bounded_String);

    type Message_Flags is mod 2 ** 16;
    type Message is tagged null record;

    --  Stats related types/functions
    --
    type Stats is new Message with null record;
    function Create return Stats;
    function Serialize(M : in Stats) return String;


    --  Get related types/functions
    --
    type Get is new Message with record
        Keys : Key_Vectors.Vector;
    end record;
    function Create(Key : in String) return Get;
    function Create(Keys : in Key_Vectors.Vector) return Get;
    function Serialize(M : in Get) return String;


    --  Exceptions
    --
    Invalid_Key_Error : exception;

end Memcache.Messages;
