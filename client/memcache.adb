--
--
with Ada.Characters.Handling;
with Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with GNAT.Sockets;
with GNAT.String_Split;

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
                return Response is
        Command : String := Generate_Get (Key);
    begin
        Write_Command (Conn => This, Command => Command);
        declare
            Reply : Response := Read_Get_Response (This);
        begin
            return Reply;
        end;
    end Get;

    function Set (This : in Connection;
                    Key : in String;
                    Value : in String;
                    Flags : in Flags_Type := 0;
                    Expire : in Expiration := 0)
                return Boolean is
        Command : String := Generate_Set (Key, Value,
                                Flags, Expire, False);
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
                    Flags : in Flags_Type := 0;
                    Expire : in Expiration := 0) is
        Unused : Boolean := Set (This, Key, Value,
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
                                Flags : in Flags_Type;
                                Expire : in Expiration;
                                No_Reply : in Boolean) return String is
        Command : Unbounded.Unbounded_String;
    begin
        Validate (Key);

        Command := Unbounded.To_Unbounded_String ("set " &
                        Key &
                        Flags_Type'Image (Flags) &
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
        End_of_Line : String := Append_CRLF ("");
    begin
        return Read_Until (Conn, End_of_Line);
    end Read_Response;

    function Read_Get_Response (Conn : in Connection) return Response is
        use GNAT.Sockets;
        use GNAT.String_Split;
        use Ada.Streams;
        Channel : Stream_Access; -- From GNAT.Sockets
        First_Line : Unbounded.Unbounded_String;
        Offset : Stream_Element_Count;
        Data   : Stream_Element_Array (1 .. 1);
        Terminator : String := Append_CRLF ("");
        Read_Char : Character;

        --  Filled in after the first line of the response is read
        Get_Flags : Flags_Type;
        Block_Length : Natural;
        Reply : Response;
    begin
        Channel := Stream (Conn.Sock);
        loop
            Read (Channel.all, Data, Offset);
            Read_Char := Character'Val (Data (1));
            Unbounded.Append (First_Line, Read_Char);

            if Contains_String (First_Line, Terminator) then
                exit;
            end if;
        end loop;

        --  The first line in the response that we get should be formatted
        --  along the lines of: VALUE <key> <flags> <length>\r\n
        --
        declare
            Subs : Slice_Set;
            Buffer : String := Unbounded.To_String (First_Line);
            --  Adust the buffer to trim the trailing ASCII.CR and ASCII.LF
            Trimmed : String := Buffer (1 .. (Buffer'Last - 2));
        begin
            Create (Subs, Trimmed, " ", Mode => Multiple);

            if Slice_Count (Subs) /= 4 then
                --  If we don't have four fields (as documented above) then
                --  we're in a real pickle.
                raise Unexpected_Response;
            end if;

            Get_Flags := Flags_Type'Value (Slice (Subs, 3));
            Block_Length := Natural'Value (Slice (Subs, 4));
        end;

        declare
            Block_Offset : Stream_Element_Count;
            --  The data to be read in the block should be of length
            --  Block_Length followed by the customary ASCII.CR and ASCII.LR
            Block_Data   : Stream_Element_Array
                                (1 .. Stream_Element_Count (Block_Length + 2));
            Block_Response : Unbounded.Unbounded_String;
        begin
            Read (Channel.all, Block_Data, Block_Offset);
            for I in 1 .. (Block_Offset - 2) loop
                Unbounded.Append (Block_Response,
                            Character'Val (Block_Data (I)));
            end loop;
            Reply.Data := Block_Response;
        end;

        return Reply;

    end Read_Get_Response;

    function Contains_String (Haystack : in Unbounded.Unbounded_String;
                    Needle : in String) return Boolean is
        R_Length : Natural := Unbounded.Length (Haystack);
        T_Length : Natural := Needle'Length;
        R_Last_Char : Character := Unbounded.Element (Haystack, R_Length);
        T_Last_Char : Character := Needle (Needle'Last);
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
                (Haystack, R_Length - (T_Length - 1), R_Length);
        begin
            if Needle = Sub then
                return True;
            end if;
        end;
        return False;
    end Contains_String;

    function Append_CRLF (Input : in String) return String is
    begin
        return Input & ASCII.CR & ASCII.LF;
    end Append_CRLF;
end Memcache;

