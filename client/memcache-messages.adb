
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
        return M;
    end Create;

    function Serialize(M : in Get) return String is
    begin
        return "get\r\n"; 
    end Serialize;
end Memcache.Messages;
