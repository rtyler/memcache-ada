--
--  Primary Ada Memcached client specification
--
--  This package defines the core API accessible by
--  users of this library
--

with Ada.Calendar;
with Ada.Containers.Vectors;
with Ada.Streams;
with Ada.Strings.Bounded;
with Ada.Strings.Unbounded;
with GNAT.Sockets;

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
    type Expiration is range 0 .. 60*60*24*30;
    subtype Port_Type is GNAT.Sockets.Port_Type;
    type Flags is mod 2 ** 16;

    type Connection is tagged private;

    function Get (This : in Connection; Key : in String)
                return Unbounded.Unbounded_String;


    function Set (This : in Connection;
                    Key : in String;
                    Set_Flags : in Flags := 0;
                    Expire : in Expiration := 0;
                    Value : in String)
                return Boolean;

    function Set (This : in Connection;
                    Key : in String;
                    Set_Flags : in Flags := 0;
                    Expire : in Ada.Calendar.Time;
                    Value : in String)
                return Boolean;

    --
    --  Functions/procedures implementing the "delete" memcached
    --  command
    function Delete (This : in Connection; Key : in String;
                    Delayed : in Expiration := 0)
                return Boolean;

    function Delete (This : in Connection; Key : in String;
                    Delayed : in Ada.Calendar.Time)
                return Boolean;

    procedure Delete (This : in Connection; Key : in String;
                    Delayed : in Expiration := 0);

    procedure Delete (This : in Connection; Key : in String;
                    Delayed : in Ada.Calendar.Time);
    --
    --


    --
    --  Functions/procedures implementing the "incr" memcached
    --  command
    function Increment (This : in Connection; Key : in String;
                    Value : in Natural)
                return Boolean;
    procedure Increment (This : in Connection; Key : in String;
                    Value : in Natural);
    --
    --


    --
    --  Functions/procedures implementing the "decr" memcached
    --  command
    function Decrement (This : in Connection; Key : in String;
                    Value : in Natural)
                return Boolean;
    procedure Decrement (This : in Connection; Key : in String;
                    Value : in Natural);
    --
    --


    --
    --  Stats from the memcached server come back in a relatively
    --  unstructured format, so this function will just dump to stdout
    procedure Dump_Stats (This : in Connection);

    function Create (Host : in String; Port : in Port_Type)
                return Connection;

    procedure Connect (Conn : in out Connection);
    procedure Disconnect (Conn : in out Connection);

    --
    --  Memcache client exceptions
    Invalid_Key_Error : exception;
    Unexpected_Response : exception;
    Invalid_Connection : exception;
    Not_Implemented : exception;

private

    type Connection is tagged record
        Sock : GNAT.Sockets.Socket_Type;
        Address : GNAT.Sockets.Sock_Addr_Type;
        Connected : Boolean := False;
    end record;


    --
    --  Validation functionsto ensure that a few basic
    --  conditions are met:
    --      * No spaces in keys
    --      * Key length is less than or equal to 250 characters
    --      * Key length is greater than zero characters
    procedure Validate (Keys : in Key_Vectors.Vector);
    procedure Validate (Key : in String);


    --
    --  Generation functions, used to generate the actual text
    --  representations of commands to be sent over the wire to
    --  the memcached server
    function Generate_Delete (Key : in String;
                                Delayed : in Expiration;
                                No_Reply : in Boolean) return String;
    function Generate_Delete (Key : in String;
                                Delayed : in Ada.Calendar.Time;
                                No_Reply : in Boolean) return String;

    function Generate_Incr (Key : in String; Value : in Natural;
                                No_Reply : in Boolean) return String;

    function Generate_Decr (Key : in String; Value : in Natural;
                                No_Reply : in Boolean) return String;


    procedure Write_Command (Conn : in Connection; Command : in String);
    function Read_Until (Conn : in Connection; Terminator : in String;
                    Trim_CRLF : in Boolean := True)
                return String;
    function Read_Response (Conn : in Connection) return String;

    function Append_CRLF (Input : in String) return String;

end Memcache;
