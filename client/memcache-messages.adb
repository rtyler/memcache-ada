
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

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
        M.Keys.Append(Bounded_Key);
        Validate(M.Keys);
        return M;
    end Create;

    function Create(Keys : in Key_Vectors.Vector) return Get is
        M : Get;
    begin
        M.Keys := Keys;
        Validate(M.Keys);
        return  M;
    end Create;

    function Serialize(M : in Get) return String is
        function Implode_Keys(M: in Get) return String is
            Length : Natural := Natural(Key_Vectors.Length(M.Keys));
            Combined_Key : Unbounded_String;
        begin
            for Index in 0 .. (Length - 1) loop
                Append(Source => Combined_Key, New_Item => Bounded.To_String(
                                                M.Keys.Element(Index => Index)));
                if Index /= (Length - 1) then
                    Append(Source => Combined_Key, New_Item => " ");
                end if;
            end loop;
            return To_String(Combined_Key);
        end Implode_Keys;
    begin
        Validate(M.Keys);
        return "get " & Implode_Keys(M) & "\r\n";
    end Serialize;


    function Create(Key : in String) return Delete is
        M : Delete;
    begin
        M.Key := Bounded.To_Bounded_String(Key);
        return M;
    end;

    function Serialize(M : in Delete) return String is
    begin
        return "delete " & Bounded.To_String(M.Key) & "\r\n";
    end Serialize;


    --  Private functions/procedures
    --
    procedure Validate(Keys : in Key_Vectors.Vector) is
        Length : Natural := Natural(Key_Vectors.Length(Keys));
    begin
        for Index in 0 .. (Length - 1) loop
            declare
                Bounded_Key : Bounded.Bounded_String := Keys.Element(Index);
            begin
                -- Cannot use empty keys
                if Bounded.Length(Bounded_Key) = 0 then
                    raise Invalid_Key_Error;
                end if;

                -- Canont use keys with spaces in them
                if Bounded.Count(Source => Bounded_Key, Pattern => " ") /= 0 then
                    raise Invalid_Key_Error;
                end if;
            end;
        end loop;
    end;
end Memcache.Messages;
