--
--
with Ada.Characters.Handling;
with Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with GNAT.Sockets;

use Ada.Strings;
use type Ada.Streams.Stream_Element_Count;

package body Memcache is
    function Create (Host : in String; Port : in Port_Type)
                return Connection is
        C : Connection;
    begin
        C.Address.Addr := GNAT.Sockets.Inet_Addr (Host);
        C.Address.Port := Port;
        return C;
    end Create;

    procedure Connect (Conn : in out Connection) is
        use GNAT.Sockets;
    begin
        if Conn.Connected then
            return;
        end if;

        Initialize;
        Create_Socket (Conn.Sock);
        Set_Socket_Option (Conn.Sock, Socket_Level, (Reuse_Address, True));
        Connect_Socket (Conn.Sock, Conn.Address);
        Conn.Connected := True;
    end Connect;

    procedure Disconnect (Conn : in out Connection) is
    begin
        GNAT.Sockets.Close_Socket (Conn.Sock);
        Conn.Connected := False;
    end Disconnect;

    function Get (This : in Connection; Key : in String)
                return Unbounded.Unbounded_String is
    begin
        raise Not_Implemented;
        return Unbounded.To_Unbounded_String ("");
    end Get;

    function Set (This : in Connection;
                    Key : in String;
                    Value : in String;
                    Set_Flags : in Flags := 0;
                    Expire : in Expiration := 0)
                return Boolean is
        Command : String := Generate_Set (Key, Value,
                                Set_Flags, Expire, False);
    begin
        Write_Command (Conn => This, Command => Command);
        declare
            Response : String := Read_Response (This);
        begin
            if Response = "STORED" then
                return True;
            end if;
            return False;
        end;
    end Set;

    procedure Set (This : in Connection;
                    Key : in String;
                    Value : in String;
                    Set_Flags : in Flags := 0;
                    Expire : in Expiration := 0) is
        Unused : Boolean := Set (This, Key, Value,
                        Set_Flags, Expire);
    begin
        null;
    end Set;

    function Set (This : in Connection;
                    Key : in String;
                    Value : in String;
                    Set_Flags : in Flags := 0;
                    Expire : in Ada.Calendar.Time)
                return Boolean is
    begin
        raise Not_Implemented;
        return False;
    end Set;


    function Delete (This : in Connection; Key : in String;
                    Delayed : in Expiration := 0)
                return Boolean is
        Command : String := Generate_Delete (Key, Delayed, False);
    begin
        Write_Command (Conn => This, Command => Command);
        declare
            Response : String := Read_Response (This);
        begin
            if Response = "DELETED" then
                return True;
            elsif Response = "NOT_FOUND" then
                return False;
            else
                raise Unexpected_Response;
            end if;
        end;
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
        Unused : Boolean := Delete (This, Key, Delayed);
    begin
        null;
    end Delete;

    procedure Delete (This : in Connection; Key : in String;
                    Delayed : in Ada.Calendar.Time) is
    begin
        raise Not_Implemented;
    end Delete;


    function Increment (This : in Connection; Key : in String;
                    Value : in Natural)
                return Boolean is
        Command : String := Generate_Incr (Key, Value, False);
    begin
        Write_Command (Conn => This, Command => Command);
        declare
            Response : String := Read_Response (This);
        begin
            if Response = "NOT_FOUND" then
                return False;
            end if;
        end;
        return True;
    end Increment;

    procedure Increment (This : in Connection; Key : in String;
                    Value : in Natural) is
        Unused : Boolean := Increment (This, Key, Value);
    begin
        null;
    end Increment;

    function Decrement (This : in Connection; Key : in String;
                    Value : in Natural)
                return Boolean is
        Command : String := Generate_Decr (Key, Value, False);
    begin
        Write_Command (Conn => This, Command => Command);
        declare
            Response : String := Read_Response (This);
        begin
            if Response = "NOT_FOUND" then
                return False;
            end if;
        end;
        return True;
    end Decrement;

    procedure Decrement (This : in Connection; Key : in String;
                    Value : in Natural) is
        Unused : Boolean := Decrement (This, Key, Value);
    begin
        null;
    end Decrement;
    --
    --  Stats from the memcached server come back in a relatively
    --  unstructured format, so this function will just dump to stdout
    procedure Dump_Stats (This : in Connection) is
        Command : String := Append_CRLF ("stats");
    begin
        Write_Command (Conn => This, Command => Command);
        declare
            Terminator : String :=  Append_CRLF ("END");
            Response : String := Read_Until (This, Terminator);
        begin
            Ada.Text_IO.Put_Line ("Stats response:");
            Ada.Text_IO.Put_Line (Response);
        end;
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
    end Validate;

    function Generate_Delete (Key : in String;
                                Delayed : in Expiration;
                                No_Reply : in Boolean) return String is
        Command : Unbounded.Unbounded_String;
    begin
        Validate (Key);

        Command := Unbounded.To_Unbounded_String ("delete ");

        if Delayed = 0 then
            Unbounded.Append (Command,
                Unbounded.To_Unbounded_String (Key));
        else
            Unbounded.Append (Command,
                Unbounded.To_Unbounded_String (Key &
                                Expiration'Image (Delayed)));
        end if;

        if No_Reply then
            Unbounded.Append (Command,
                Unbounded.To_Unbounded_String (" noreply"));
        end if;

        return Unbounded.To_String (Command) & ASCII.CR & ASCII.LF;
    end Generate_Delete;

    function Generate_Delete (Key : in String;
                                Delayed : in Ada.Calendar.Time;
                                No_Reply : in Boolean) return String is
    begin
        Validate (Key);
        return  "";
    end Generate_Delete;


    function Generate_Incr (Key : in String;
                                Value : in Natural;
                                No_Reply : in Boolean) return String is
        Command : Unbounded.Unbounded_String;
    begin
        Validate (Key);

        --  NOTE: Integer'Image leaves a preceding space *or* a minus
        --  sign, since this is a Natural type, we won't have a preceding
        --  minus sign so we'll use that preceding space for formatting the
        --  command
        Command := Unbounded.To_Unbounded_String ("incr " &
                    Key & Natural'Image (Value));

        if No_Reply then
            Unbounded.Append (Command,
                Unbounded.To_Unbounded_String (" noreply"));
        end if;

        return Unbounded.To_String (Command) & ASCII.CR & ASCII.LF;
    end Generate_Incr;


    function Generate_Decr (Key : in String;
                                Value : in Natural;
                                No_Reply : in Boolean) return String is
        Command : Unbounded.Unbounded_String;
    begin
        Validate (Key);

        --  NOTE: Integer'Image leaves a preceding space *or* a minus
        --  sign, since this is a Natural type, we won't have a preceding
        --  minus sign so we'll use that preceding space for formatting the
        --  command
        Command := Unbounded.To_Unbounded_String ("decr " &
                    Key & Natural'Image (Value));

        if No_Reply then
            Unbounded.Append (Command,
                Unbounded.To_Unbounded_String (" noreply"));
        end if;

        return Unbounded.To_String (Command) & ASCII.CR & ASCII.LF;
    end Generate_Decr;


    function Generate_Set (Key : in String; Value : in String;
                                Set_Flags : in Flags;
                                Expire : in Expiration;
                                No_Reply : in Boolean) return String is
        Command : Unbounded.Unbounded_String;
    begin
--<command name> <key> <flags> <exptime> <bytes> [noreply]\r\n
        Validate (Key);

        Command := Unbounded.To_Unbounded_String ("set " &
                        Key &
                        Flags'Image (Set_Flags) &
                        Expiration'Image (Expire) &
                        Natural'Image (Value'Length) &
                        ASCII.CR & ASCII.LF);
        Unbounded.Append (Command, Append_CRLF (Value));

        if No_Reply then
            Unbounded.Append (Command,
                Unbounded.To_Unbounded_String (" noreply"));
        end if;

        return Unbounded.To_String (Command);
    end Generate_Set;


    procedure Write_Command (Conn : in Connection; Command : in String) is
        use GNAT.Sockets;
        use Ada.Streams;
        Channel : Stream_Access; -- From GNAT.Sockets
    begin
        if Conn.Connected = False then
            raise Invalid_Connection;
        end if;

        Channel := Stream (Conn.Sock);
        String'Write (Channel, Command);

    end Write_Command;

    function Read_Until (Conn : in Connection; Terminator : in String;
                    Trim_CRLF : in Boolean := True)
                return String is
        use GNAT.Sockets;
        use Ada.Streams;
        Channel : Stream_Access; -- From GNAT.Sockets
        Response : Unbounded.Unbounded_String;
        Offset : Stream_Element_Count;
        Data   : Stream_Element_Array (1 .. 1);
        R_Length : Natural;
        Read_Char : Character;

        function Should_Exit (Buffer : in Unbounded.Unbounded_String;
                        Terminator : in String) return Boolean is
            R_Length : Natural := Unbounded.Length (Buffer);
            T_Length : Natural := Terminator'Length;
            R_Last_Char : Character := Unbounded.Element (Buffer, R_Length);
            T_Last_Char : Character := Terminator (Terminator'Last);
        begin
            if R_Length < T_Length then
                return False;
            end if;

            --  Only check the last N characters if the current
            --  character matches the last one in the Terminator
            if R_Last_Char /= T_Last_Char then
                return False;
            end if;

            declare
                Sub : String := Unbounded.Slice
                    (Buffer, R_Length - (T_Length - 1), R_Length);
            begin
                if Terminator = Sub then
                    return True;
                end if;
            end;
            return False;
        end Should_Exit;

    begin
        Channel := Stream (Conn.Sock);
        loop
            Read (Channel.All, Data, Offset);
            Read_Char := Character'Val (Data (1));
            Unbounded.Append (Response, Read_Char);

            if Should_Exit (Response, Terminator) then
                exit;
            end if;
        end loop;

        R_Length := Unbounded.Length (Response);

        if Trim_CRLF then
            --  Return the string with the CR + LF sliced off the end
            return Unbounded.Slice (Response, 1, R_Length - 2);
        else
            return Unbounded.To_String (Response);
        end if;
    end Read_Until;

    function Read_Response (Conn : in Connection) return String is
        End_of_Line : String := Append_CRLF ("");
    begin
        return Read_Until (Conn, End_of_Line);
    end Read_Response;

    function Append_CRLF (Input : in String) return String is
    begin
        return Input & ASCII.CR & ASCII.LF;
    end Append_CRLF;
end Memcache;

