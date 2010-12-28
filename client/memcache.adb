--

private with Ada.Calendar;
private with Ada.Characters.Handling;
private with Ada.Streams;
private with Ada.Text_IO;

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


    function Delete (This : in Connection; Key : in String;
                    Delayed : in Expiration := 0.0)
                return Boolean is
        Command : constant String := Generate_Delete (Key, Delayed, False);
    begin
        Is_Connected (This);
        Write_Command (Conn => This, Command => Command);
        declare
            Response : constant String := Read_Response (This);
        begin
            if Response = Response_Deleted then
                return True;
            elsif Response = Response_Not_Found then
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
                    Delayed : in Expiration := 0.0) is
        Unused : constant Boolean := Delete (This, Key, Delayed);
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
        Command : constant String := Generate_Incr (Key, Value, False);
    begin
        Is_Connected (This);
        Write_Command (Conn => This, Command => Command);
        declare
            Response : constant String := Read_Response (This);
        begin
            if Response = Response_Not_Found then
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
        Command : constant String := Generate_Decr (Key, Value, False);
    begin
        Is_Connected (This);
        Write_Command (Conn => This, Command => Command);
        declare
            Response : constant String := Read_Response (This);
        begin
            if Response = Response_Not_Found then
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
                Key_Ch : constant Character := Key (Index);
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

    procedure Is_Connected (C : in Connection) is
    begin
        if C.Connected then
            return;
        end if;

        raise Not_Connected;
    end Is_Connected;

    function Generate_Delete (Key : in String;
                                Delayed : in Expiration;
                                No_Reply : in Boolean) return String is
        Command : Unbounded.Unbounded_String;
    begin
        Validate (Key);

        Command := Unbounded.To_Unbounded_String ("delete " & Key);

        if Delayed /= 0.0 then
            Unbounded.Append (Command,
                    Natural'Image (Natural (Delayed)));
        end if;

        if No_Reply then
            Unbounded.Append (Command,
                Unbounded.To_Unbounded_String (" noreply"));
        end if;

        return Append_CRLF (Unbounded.To_String (Command));
    end Generate_Delete;

    function Generate_Delete (Key : in String;
                                Delayed : in Ada.Calendar.Time;
                                No_Reply : in Boolean) return String is
        Command : Unbounded.Unbounded_String;
        Delayed_Since_Epoch : constant Natural := Natural (Delayed - Epoch);
    begin
        Validate (Key);

        Command := Unbounded.To_Unbounded_String ("delete " & Key);

        Unbounded.Append (Command, Natural'Image (Delayed_Since_Epoch));

        if No_Reply then
            Unbounded.Append (Command,
                Unbounded.To_Unbounded_String (" noreply"));
        end if;

        return Append_CRLF (Unbounded.To_String (Command));
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

        return Append_CRLF (Unbounded.To_String (Command));
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

        return Append_CRLF (Unbounded.To_String (Command));
    end Generate_Decr;


    function Generate_Store (Kind : in Store_Commands;
                                Key : in String;
                                Value : in String;
                                Flags : in Flags_Type;
                                Expire : in Natural;
                                No_Reply : in Boolean) return String is
        Command : Unbounded.Unbounded_String;
    begin
        Validate (Key);

        case Kind is
            when Set =>
                Command := Unbounded.To_Unbounded_String ("set ");
            when Add =>
                Command := Unbounded.To_Unbounded_String ("add ");
            when Append =>
                Command := Unbounded.To_Unbounded_String ("append ");
            when Prepend =>
                Command := Unbounded.To_Unbounded_String ("prepend ");
            when Replace =>
                Command := Unbounded.To_Unbounded_String ("replace ");
        end case;

        Unbounded.Append (Command, Unbounded.To_Unbounded_String (
                        Key &
                        Flags_Type'Image (Flags) &
                        Natural'Image (Expire) &
                        Natural'Image (Value'Length) &
                        CRLF &
                        Append_CRLF (Value)));

        if No_Reply then
            Unbounded.Append (Command,
                Unbounded.To_Unbounded_String (" noreply"));
        end if;

        return Unbounded.To_String (Command);
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
        Response : Unbounded.Unbounded_String;
        Offset : Stream_Element_Count;
        Data   : Stream_Element_Array (1 .. 1);
        R_Length : Natural;
        Read_Char : Character;
    begin
        Channel := Stream (Conn.Sock);
        loop
            Read (Channel.all, Data, Offset);
            Read_Char := Character'Val (Data (1));
            Unbounded.Append (Response, Read_Char);

            if Contains_String (Response, Terminator) then
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
    begin
        return Read_Until (Conn, CRLF);
    end Read_Response;

    function Read_Get_Response (Conn : in Connection)
                    return Response is separate;

    function Contains_String (Haystack : in Unbounded.Unbounded_String;
                    Needle : in String) return Boolean is separate;
    function Append_CRLF (Input : in String) return String is
    begin
        return Input & CRLF;
    end Append_CRLF;
end Memcache;

