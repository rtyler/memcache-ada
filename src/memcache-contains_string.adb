separate (Memcache)
    function Contains_String (Haystack : in SU.Unbounded_String;
                    Needle : in String) return Boolean is
        R_Length : constant Natural := SU.Length (Haystack);
        T_Length : constant Natural := Needle'Length;
        R_Last_Char : constant Character := SU.Element
                                                (Haystack, R_Length);
        T_Last_Char : constant Character := Needle (Needle'Last);
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
            Sub : constant String := SU.Slice
                (Haystack, R_Length - (T_Length - 1), R_Length);
        begin
            if Needle = Sub then
                return True;
            end if;
        end;
        return False;
    end Contains_String;

