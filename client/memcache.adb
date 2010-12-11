--
--


package body Memcache is
    function Get (This : in Connection; Key : in String)
                return Unbounded.Unbounded_String is
    begin
        raise Not_Implemented;
        return Unbounded.To_Unbounded_String ("");
    end Get;

    function Set (This : in Connection;
                    Key : in String;
                    Set_Flags : in Flags := 0;
                    Expire : in Expiration := 0;
                    Value : in String)
                return Boolean is
    begin
        raise Not_Implemented;
        return False;
    end Set;

    function Set (This : in Connection;
                    Key : in String;
                    Set_Flags : in Flags := 0;
                    Expire : in Ada.Calendar.Time;
                    Value : in String)
                return Boolean is
    begin
        raise Not_Implemented;
        return False;
    end Set;


    function Delete (This : in Connection; Key : in String;
                    Delayed : in Expiration := 0)
                return Boolean is
    begin
        raise Not_Implemented;
        return False;
    end Delete;

    function Delete (This : in Connection; Key : in String;
                    Delayed : in Ada.Calendar.Time)
                return Boolean is
    begin
        raise Not_Implemented;
        return False;
    end Delete;

    procedure Delete (This : in Connection; Key : in String;
                    Delayed : in Expiration := 0) is
    begin
        raise Not_Implemented;
    end Delete;

    procedure Delete (This : in Connection; Key : in String;
                    Delayed : in Ada.Calendar.Time) is
    begin
        raise Not_Implemented;
    end Delete;


    function Increment (This : in Connection; Key : in String;
                    Value : in Natural)
                return Boolean is
    begin
        raise Not_Implemented;
        return False;
    end Increment;

    procedure Increment (This : in Connection; Key : in String;
                    Value : in Natural) is
    begin
        raise Not_Implemented;
    end Increment;

    function Decrement (This : in Connection; Key : in String;
                    Value : in Natural)
                return Boolean is
    begin
        raise Not_Implemented;
        return False;
    end Decrement;

    procedure Decrement (This : in Connection; Key : in String;
                    Value : in Natural) is
    begin
        raise Not_Implemented;
    end Decrement;
    --
    --  Stats from the memcached server come back in a relatively
    --  unstructured format, so this function will just dump to stdout
    procedure Dump_Stats (This : in Connection) is
    begin
        raise Not_Implemented;
    end Dump_Stats;


    --
    --  Private functions/procedures
    --
    procedure Validate (Keys : in Key_Vectors.Vector) is
    begin
        raise Not_Implemented;
    end Validate;

    procedure Validate (Key : in Bounded.Bounded_String) is
    begin
        raise Not_Implemented;
    end Validate;

end Memcache;

