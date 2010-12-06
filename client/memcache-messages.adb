
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
    begin
        M.Keys.Append(Bounded.To_Bounded_String(Key));
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
