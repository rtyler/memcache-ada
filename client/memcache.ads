--
--  Primary Ada Memcached client specification
--
--  This package defines the core API accessible by
--  users of this library
--

with Ada.Strings.Bounded;
with Ada.Strings.Unbounded;

package Memcache is
    package Unbounded renames Ada.Strings.Unbounded;
    package Bounded is new
            Ada.Strings.Bounded.Generic_Bounded_Length (Max => 250);

    type Connection is tagged private;

    function Get (This : in Connection; Key : in String) return Unbounded.Unbounded_String;
    function Delete (This : in Connection; Key : in String) return Boolean;

    procedure Dump_Stats (This : in Connection);


private

    type Connection is tagged record
        Host : Unbounded.Unbounded_String;
        Port : Natural;
    end record;

end Memcache;
