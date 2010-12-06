
with Ada.Calendar;
with Ada.Text_IO;

use Ada.Calendar;


package body Memcache.Messages is
    function Create return Stats is
        M : Stats;
    begin
        return M;
    end Create;

    function Serialize(M : in Stats) return String is
    begin
        return "stats\r\n";
    end Serialize;


    function Create(Key : in String) return Get is 
        M : Get;
        Bounded_Key : Bounded.Bounded_String := Bounded.To_Bounded_String(Key);
    begin
        -- Cannot use empty keys
        if Key'Length = 0 then
            raise Invalid_Key_Error;
        end if;

        -- Canont use keys with spaces in them
        if Bounded.Count(Source => Bounded_Key, Pattern => " ") /= 0 then
            raise Invalid_Key_Error;
        end if;

        M.Keys.Append(Bounded_Key);
        return M;
    end Create;

    function Serialize(M : in Get) return String is
        function Implode_Keys(M: in Get) return String is
            Length : Natural := Natural(Key_Vectors.Length(M.Keys));
        begin
            if Length = 0 then
                return "";
            elsif Length = 1 then
                return Bounded.To_String(M.Keys.Element(Index => 0));
            else
                return "";
            end if;
        end Implode_Keys;
    begin
        return "get " & Implode_Keys(M) & "\r\n";
    end Serialize;
end Memcache.Messages;
