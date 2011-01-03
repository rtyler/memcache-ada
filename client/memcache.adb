--

private with Ada.Calendar;
private with Ada.Characters.Handling;
private with Ada.Streams;
private with Ada.Text_IO;
private with GNAT.String_Split;

use type Ada.Streams.Stream_Element_Count;
use type Ada.Calendar.Time;

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
                return Response is
        Command : constant String := Generate_Get (Key);
    begin
        Is_Connected (This);
        Write_Command (Conn => This, Command => Command);
        declare
            Reply : constant Response := Read_Get_Response (This);
        begin
            return Reply;
        end;
    end Get;

    function Set (This : in Connection;
                    Key : in String;
                    Value : in String;
                    Flags : in Flags_Type := 0;
                    Expire : in Expiration := 0.0)
                return Boolean is
        Command : constant String := Generate_Store (Set, Key, Value,
                                Flags, Expire, False);
    begin
        Is_Connected (This);
        Write_Command (Conn => This, Command => Command);
        declare
            Response : constant String := Read_Response (This);
        begin
            if Response = Response_Stored  then
                return True;
            end if;
            return False;
        end;
    end Set;

    procedure Set (This : in Connection;
                    Key : in String;
                    Value : in String;
                    Flags : in Flags_Type := 0;
                    Expire : in Expiration := 0.0) is
        Unused : constant Boolean := Set (This, Key, Value,
                        Flags, Expire);
    begin
        null;
    end Set;

    function Set (This : in Connection;
                    Key : in String;
                    Value : in String;
                    Flags : in Flags_Type := 0;
                    Expire : in Ada.Calendar.Time)
                return Boolean is
    begin
        raise Not_Implemented;
        return False;
    end Set;



    --
    --  DELETE PROCEDURES
    --
    procedure Exec_Delete (This : in Connection;
                            Command : in String;
                            Success : out Boolean) is
    begin
        Success := False;
        Is_Connected (This);
        Write_Command (Conn => This, Command => Command);

        declare
            Response : constant String := Read_Response (This);
        begin
            if Response = Response_Deleted then
                Success := True;
                return;
            elsif Response = Response_Not_Found then
                return;
            else
                raise Unexpected_Response;
            end if;
        end;
    end Exec_Delete;

    procedure Delete (This : in Connection;
                    Key : in String;
                    Delayed : in Expiration := 0.0;
                    Success : out Boolean) is
        Command : constant String := Generate_Delete (Key, Delayed, False);
    begin
        Exec_Delete (This, Command, Success);
    end Delete;

    procedure Delete (This : in Connection;
                    Key : in String;
                    Delayed : in Ada.Calendar.Time;
                    Success : out Boolean) is
        Command : constant String := Generate_Delete (Key, Delayed, False);
    begin
        Exec_Delete (This, Command, Success);
    end Delete;

    procedure Delete (This : in Connection; Key : in String;
                    Delayed : in Expiration := 0.0) is
        Unused : Boolean;
    begin
        Delete (This, Key, Delayed, Unused);
    end Delete;

    procedure Delete (This : in Connection; Key : in String;
                    Delayed : in Ada.Calendar.Time) is
        Unused : Boolean;
    begin
        Delete (This, Key, Delayed, Unused);
    end Delete;
    --
    --  DELETE PROCEDURES
    --


    procedure Increment (This : in Connection; Key : in String;
                    Value : in Natural;
                    Result : out Natural) is
        Command : constant String := Generate_Incr (Key, Value, False);
    begin
        Is_Connected (This);
        Write_Command (Conn => This, Command => Command);
        declare
            Response : constant String := Read_Response (This);
        begin
            if Response = Response_Not_Found then
                Result := 0;
                return;
            end if;

            Result := Natural'Value (Response);
        end;
    end Increment;

    procedure Increment (This : in Connection; Key : in String;
                    Value : in Natural) is
        Unused : Natural := 0;
    begin
        Increment (This, Key, Value, Unused);
    end Increment;

    procedure Decrement (This : in Connection;
                    Key : in String;
                    Value : in Natural;
                    Success : out Boolean) is
        Command : constant String := Generate_Decr (Key, Value, False);
    begin
        Success := True;
        Is_Connected (This);
        Write_Command (Conn => This, Command => Command);
        declare
            Response : constant String := Read_Response (This);
        begin
            if Response = Response_Not_Found then
                Success := False;
                return;
            end if;
        end;
    end Decrement;



    procedure Flush_All (This : in Connection) is
    begin
        Is_Connected (This);
        Write_Command (Conn => This, Command => Flush_All_Command);
    end Flush_All;



    function Version (This : in Connection) return String is
    begin
        Is_Connected (This);
        Write_Command (Conn => This, Command => Version_Command);
        declare
            use GNAT.String_Split;
            Response : constant String := Read_Response (This);
            Pieces : Slice_Set;
        begin
            --  GNAT.String_Split.Create
            Create (Pieces, Response, " ", Mode => Multiple);

            --
            --  The Response we get from the server is in the form of "VERSION
            --  x.x.x", we only want the last bit so we need to chop it up
            --
            return String (Slice (Pieces, 2));
        end;
    end Version;



    procedure Decrement (This : in Connection; Key : in String;
                    Value : in Natural) is
        Unused : Boolean := False;
    begin
        Decrement (This, Key, Value, Unused);
    end Decrement;
    --
    --  Stats from the memcached server come back in a relatively
    --  unstructured format, so this function will just dump to stdout
    procedure Dump_Stats (This : in Connection) is
        Command : constant String := Append_CRLF ("stats");
    begin
        Is_Connected (This);
        Write_Command (Conn => This, Command => Command);
        declare
            Terminator : constant String :=  Append_CRLF ("END");
            Response : constant String := Read_Until (This, Terminator);
        begin
            Ada.Text_IO.Put_Line ("Stats response:");
            Ada.Text_IO.Put_Line (Response);
        end;
    end Dump_Stats;


    --
    --  Private functions/procedures
    --
    procedure Validate (Key : in String) is separate;

    procedure Is_Connected (C : in Connection) is
    begin
        if C.Connected then
            return;
        end if;

        raise Not_Connected;
    end Is_Connected;

    function Generate_Delete (Key : in String;
                                Delayed : in Natural;
                                No_Reply : in Boolean) return String is
        Command : SU.Unbounded_String;
    begin
        Validate (Key);

        Command := SU.To_Unbounded_String ("delete " & Key);

        if Delayed /= 0 then
            SU.Append (Command,
                    Natural'Image (Delayed));
        end if;

        if No_Reply then
            SU.Append (Command, Unbounded_No_Reply);
        end if;

        return Append_CRLF (SU.To_String (Command));
    end Generate_Delete;

    function Generate_Delete (Key : in String;
                                Delayed : in Expiration;
                                No_Reply : in Boolean) return String is
        Natural_Expire : constant Natural := Natural (Delayed);
    begin
        return Generate_Delete (Key, Natural_Expire, No_Reply);
    end Generate_Delete;

    function Generate_Delete (Key : in String;
                                Delayed : in Ada.Calendar.Time;
                                No_Reply : in Boolean) return String is
        Delayed_Since_Epoch : constant Natural := Natural (Delayed - Epoch);
    begin
        return Generate_Delete (Key, Delayed_Since_Epoch, No_Reply);
    end Generate_Delete;


    function Generate_Incr (Key : in String;
                                Value : in Natural;
                                No_Reply : in Boolean) return String is
        Command : SU.Unbounded_String;
    begin
        Validate (Key);

        --  NOTE: Integer'Image leaves a preceding space *or* a minus
        --  sign, since this is a Natural type, we won't have a preceding
        --  minus sign so we'll use that preceding space for formatting the
        --  command
        Command := SU.To_Unbounded_String ("incr " &
                    Key & Natural'Image (Value));

        if No_Reply then
            SU.Append (Command, Unbounded_No_Reply);
        end if;

        return Append_CRLF (SU.To_String (Command));
    end Generate_Incr;


    function Generate_Decr (Key : in String;
                                Value : in Natural;
                                No_Reply : in Boolean) return String is
        Command : SU.Unbounded_String;
    begin
        Validate (Key);

        --  NOTE: Integer'Image leaves a preceding space *or* a minus
        --  sign, since this is a Natural type, we won't have a preceding
        --  minus sign so we'll use that preceding space for formatting the
        --  command
        Command := SU.To_Unbounded_String ("decr " &
                    Key & Natural'Image (Value));

        if No_Reply then
            SU.Append (Command, Unbounded_No_Reply);
        end if;

        return Append_CRLF (SU.To_String (Command));
    end Generate_Decr;


    function Generate_Store (Kind : in Store_Commands;
                                Key : in String;
                                Value : in String;
                                Flags : in Flags_Type;
                                Expire : in Natural;
                                No_Reply : in Boolean) return String is
        Command : SU.Unbounded_String;
    begin
        Validate (Key);

        case Kind is
            when Set =>
                Command := SU.To_Unbounded_String ("set ");
            when Add =>
                Command := SU.To_Unbounded_String ("add ");
            when Append =>
                Command := SU.To_Unbounded_String ("append ");
            when Prepend =>
                Command := SU.To_Unbounded_String ("prepend ");
            when Replace =>
                Command := SU.To_Unbounded_String ("replace ");
        end case;

        SU.Append (Command, SU.To_Unbounded_String (
                        Key &
                        Flags_Type'Image (Flags) &
                        Natural'Image (Expire) &
                        Natural'Image (Value'Length) &
                        CRLF &
                        Append_CRLF (Value)));

        if No_Reply then
            SU.Append (Command, Unbounded_No_Reply);
        end if;

        return SU.To_String (Command);
    end Generate_Store;

    function Generate_Store (Kind : in Store_Commands;
                                Key : in String;
                                Value : in String;
                                Flags : in Flags_Type;
                                Expire : in Expiration;
                                No_Reply : in Boolean) return String is
        Natural_Expire : constant Natural := Natural (Expire);
    begin
        return Generate_Store (Kind, Key, Value,
                            Flags, Natural_Expire, No_Reply);
    end Generate_Store;

    function Generate_Store (Kind : in Store_Commands;
                                Key : in String;
                                Value : in String;
                                Flags : in Flags_Type;
                                Expire : in Ada.Calendar.Time;
                                No_Reply : in Boolean) return String is
        Expire_Since_Epoch : constant Natural := Natural (Expire - Epoch);
    begin
        return Generate_Store (Kind, Key, Value,
                            Flags, Expire_Since_Epoch, No_Reply);
    end Generate_Store;

    function Generate_Get (Key : in String) return String is
    begin
        Validate (Key);

        return Append_CRLF ("get " & Key);
    end Generate_Get;

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
        Response : SU.Unbounded_String;
        Offset : Stream_Element_Count;
        Data   : Stream_Element_Array (1 .. 1);
        R_Length : Natural;
        Read_Char : Character;
    begin
        Channel := Stream (Conn.Sock);
        loop
            Read (Channel.all, Data, Offset);
            Read_Char := Character'Val (Data (1));
            SU.Append (Response, Read_Char);

            if Contains_String (Response, Terminator) then
                exit;
            end if;
        end loop;

        R_Length := SU.Length (Response);

        if Trim_CRLF then
            --  Return the string with the CR + LF sliced off the end
            return SU.Slice (Response, 1, R_Length - 2);
        else
            return SU.To_String (Response);
        end if;
    end Read_Until;

    function Read_Response (Conn : in Connection) return String is
    begin
        return Read_Until (Conn, CRLF);
    end Read_Response;

    function Read_Get_Response (Conn : in Connection)
                    return Response is separate;

    function Contains_String (Haystack : in SU.Unbounded_String;
                    Needle : in String) return Boolean is separate;
    function Append_CRLF (Input : in String) return String is
    begin
        return Input & CRLF;
    end Append_CRLF;
end Memcache;

