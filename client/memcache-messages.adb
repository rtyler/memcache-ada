
with Ada.Calendar;
with Ada.Text_IO;

use Ada.Calendar;


package body Memcache.Messages is

    function Serialize (Time : Ada.Calendar.Time) return String is
    begin -- Serialize
        return "";
    end Serialize;

    function Serialize (Flags : Message_Flags) return String is
    begin -- Serialize
        return "";
    end Serialize;

    function Image (N : Natural) return String is
    begin -- Image
        -- Basically, just trim space from Natural'Image result
        return "";
    end Image;

    function Serialize (Message_In : in Message) return String is
    begin
        case Message_In.C is
            when Stats =>
                return "stats\r\n";
            when others =>
                return "";
        end case;
        --return Command'Image (Message_In.C) & " " &
        --       Serialize (Message_In.Flags) & " " &
        --       To_Unix_Time (Message_In.Expire_At) & " " &
        --       Image (Message_In.Bytes);
    end Serialize;

end Memcache.Messages;
