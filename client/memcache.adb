--
--
with Ada.Strings.Fixed;
with Ada.Characters.Handling;

use Ada.Strings;

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

    procedure Validate (Key : in String) is
    begin
        --  A key must be between 1 and 250 characters in length
        if Key'Length = 0  or Key'Length > 250 then
            raise Invalid_Key_Error;
        end if;

        --
        --  It's unfortunate that string scanning is apprently necessary,
        --  but we need to verify a few conditions for the string:
        --      * No whitespace
        --      * No control characters
        for Index in Key'Range loop
            declare
                Key_Ch : Character := Key (Index);
            begin
                if Character'Pos (Key_Ch) = 32 then
                    raise Invalid_Key_Error;
                end if;

                if Ada.Characters.Handling.Is_Control (Key_Ch) then
                    raise Invalid_Key_Error;
                end if;
            end;
        end loop;

        raise Not_Implemented;
    end Validate;

end Memcache;

