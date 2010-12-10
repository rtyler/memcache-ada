--
--  Primary Ada Memcached client specification
--
--  This package defines the core API accessible by
--  users of this library
--

with Ada.Calendar;
with Ada.Containers.Vectors;
with Ada.Strings.Bounded;
with Ada.Strings.Unbounded;

use Ada.Containers;
use Ada.Strings.Bounded;

package Memcache is
    package Unbounded renames Ada.Strings.Unbounded;
    package Bounded is new Generic_Bounded_Length (Max => 250);
    use type Bounded.Bounded_String; -- Pull in operators for Bounded_String
    package Key_Vectors is new Vectors (Natural, Bounded.Bounded_String);

    --
    --  When passing an expiration, it must be within this range
    --  otherwise the server will treat it as a unix timestamp
    type Expiration is range 1 .. 60*60*24*30;

    type Connection is tagged private;

    function Get (This : in Connection; Key : in String)
                return Unbounded.Unbounded_String;
    function Gets (This : in Connection; Keys : in Key_Vectors.Vector)
                return Boolean;


    function Delete (This : in Connection; Key : in String)
                return Boolean;
    function Delete (This : in Connection; Key : in String;
                    Delayed : in Expiration) return Boolean;
    function Delete (This : in Connection; Key : in String;
                    Delayed : in Ada.Calendar.Time) return Boolean;
    procedure Delete (This : in Connection; Key : in String);
    procedure Delete (This : in Connection; Key : in String;
                    Delayed : in Expiration);
    procedure Delete (This : in Connection; Key : in String;
                    Delayed : in Ada.Calendar.Time);

    function Set (This : in Connection; Key : in String;
                    Value : in String)
                return Boolean;



    --
    --  Stats from the memcached server come back in a relatively
    --  unstructured format, so this function will just dump to stdout
    procedure Dump_Stats (This : in Connection);


    --
    --  Memcache client exceptions
    Invalid_Key_Error : exception;

private

    type Connection is tagged record
        Host : Unbounded.Unbounded_String;
        Port : Natural;
    end record;


    --
    --  Validation functionsto ensure that a few basic
    --  conditions are met:
    --      * No spaces in keys
    --      * Key length is less than or equal to 250 characters
    --      * Key length is greater than zero characters
    procedure Validate (Keys : in Key_Vectors.Vector);
    procedure Validate (Key : in Bounded.Bounded_String);

end Memcache;
