private with GNAT.Sockets;
private with GNAT.String_Split;
private with Ada.Streams;

separate (Memcache)
    function Read_Get_Response (Conn : in Connection) return Response is
        use GNAT.Sockets;
        use GNAT.String_Split;
        use Ada.Streams;
        Channel : Stream_Access; -- From GNAT.Sockets
        First_Line : Unbounded.Unbounded_String;
        Offset : Stream_Element_Count;
        Data   : Stream_Element_Array (1 .. 1);
        Terminator : constant String := CRLF;
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
            Buffer : constant String := Unbounded.To_String (First_Line);
            --  Adust the buffer to trim the trailing ASCII.CR and ASCII.LF
            Trimmed : constant String := Buffer (1 .. (Buffer'Last - 2));
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
