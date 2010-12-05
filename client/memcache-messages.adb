with Ada.Text_IO;


package body Memcache.Messages is

    function Serialize(Message_In: in Message) return String is
    begin
        return To_String(Message_In.Raw_Command);
    end Serialize;

end Memcache.Messages;
