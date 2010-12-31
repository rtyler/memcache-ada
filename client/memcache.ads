--
--  Primary Ada Memcached client specification
--
--  This package defines the core API accessible by
--  users of this library
--

with Ada.Calendar;
with Ada.Strings.Unbounded;
with GNAT.Sockets;

private with Ada.Characters.Latin_1;

package Memcache is
    package SU renames Ada.Strings.Unbounded;
    use type SU.Unbounded_String;

    --
    --  When passing an expiration, it must be within this range
    --  otherwise the server will treat it as a unix timestamp
    subtype Expiration is Duration range 0.0 .. 2592000.0;
    subtype Port_Type is GNAT.Sockets.Port_Type;
    type Flags_Type is mod 2 ** 16;

    type Response is record
        Flags : Flags_Type;
        Data : SU.Unbounded_String;
    end record;

    type Connection is tagged private;

    function Get (This : in Connection; Key : in String)
                return Response;

    function Set (This : in Connection;
                    Key : in String;
                    Value : in String;
                    Flags  : in Flags_Type := 0;
                    Expire : in Expiration := 0.0)
                return Boolean;

    procedure Set (This : in Connection;
                    Key : in String;
                    Value : in String;
                    Flags : in Flags_Type := 0;
                    Expire : in Expiration := 0.0);

    function Set (This : in Connection;
                    Key : in String;
                    Value : in String;
                    Flags : in Flags_Type := 0;
                    Expire : in Ada.Calendar.Time)
                return Boolean;

    --
    --  Functions/procedures implementing the "delete" memcached
    --  command
    procedure Delete (This : in Connection; Key : in String;
                    Delayed : in Expiration := 0.0);
    procedure Delete (This : in Connection; Key : in String;
                    Delayed : in Expiration := 0.0;
                    Success : out Boolean);

    procedure Delete (This : in Connection; Key : in String;
                    Delayed : in Ada.Calendar.Time);
    procedure Delete (This : in Connection; Key : in String;
                    Delayed : in Ada.Calendar.Time;
                    Success : out Boolean);
    --
    --


    --
    --  `Increment` the value for a key in memcached, the server will assume
    --  the type of the value to be a 64-bit integer and increment it the
    --  desired `Value` passed
    procedure Increment (This : in Connection;
                    Key : in String;
                    Value : in Natural);
    procedure Increment (This : in Connection;
                    Key : in String;
                    Value : in Natural;
                    Result : out Natural);


    --
    --  Functions/procedures implementing the "decr" memcached
    --  command
    procedure Decrement (This : in Connection;
                    Key : in String;
                    Value : in Natural);
    procedure Decrement (This : in Connection;
                    Key : in String;
                    Value : in Natural;
                    Success : out Boolean);
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
    Not_Connected : exception;

private

    Response_Stored : constant String := "STORED";
    Response_Deleted : constant String := "DELETED";
    Response_Not_Found : constant String := "NOT_FOUND";
    Epoch : constant Ada.Calendar.Time :=
                        Ada.Calendar.Time_Of (1970, 1, 1);
    type Store_Commands is (Set, Add, Replace, Append, Prepend);

    CRLF : constant String := Ada.Characters.Latin_1.CR &
                                Ada.Characters.Latin_1.LF;
    Unbounded_No_Reply : constant SU.Unbounded_String :=
                            SU.To_Unbounded_String (" noreply");

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
    procedure Validate (Key : in String);

    --
    --  Verify that our Connection object has not been previously
    --  disconnected, will raise `Not_Connected` if it has
    procedure Is_Connected (C : in Connection);


    procedure Exec_Delete (This : in Connection;
                            Command : in String;
                            Success : out Boolean);
    --
    --  Generation functions, used to generate the actual text
    --  representations of commands to be sent over the wire to
    --  the memcached server
    function Generate_Delete (Key : in String;
                                Delayed : in Natural;
                                No_Reply : in Boolean) return String;
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


    function Generate_Store (Kind : in Store_Commands;
                                Key : in String;
                                Value : in String;
                                Flags : in Flags_Type;
                                Expire : in Natural;
                                No_Reply : in Boolean) return String;
    function Generate_Store (Kind : in Store_Commands;
                                Key : in String;
                                Value : in String;
                                Flags : in Flags_Type;
                                Expire : in Expiration;
                                No_Reply : in Boolean) return String;
    function Generate_Store (Kind : in Store_Commands;
                                Key : in String;
                                Value : in String;
                                Flags : in Flags_Type;
                                Expire : in Ada.Calendar.Time;
                                No_Reply : in Boolean) return String;


    function Generate_Get (Key : in String) return String;

    procedure Write_Command (Conn : in Connection; Command : in String);
    function Read_Until (Conn : in Connection; Terminator : in String;
                    Trim_CRLF : in Boolean := True)
                return String;
    function Read_Response (Conn : in Connection) return String;
    function Read_Get_Response (Conn : in Connection) return Response;

    function Contains_String (Haystack : in SU.Unbounded_String;
                    Needle : in String) return Boolean;

    --
    --  Append a \r\n on to the Input string
    function Append_CRLF (Input : in String) return String;

end Memcache;
