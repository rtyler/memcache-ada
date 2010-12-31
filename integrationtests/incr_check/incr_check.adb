--
--  Test executable to create a connection, execute a `delete`
--  command and then disconnect.
--

with Ada.Command_Line;
with Memcache;

procedure Incr_Check is
    Port : String := Ada.Command_Line.Argument (1);
    C : Memcache.Connection :=
            Memcache.Create (Host => "127.0.0.1",
                            Port => Memcache.Port_Type'Value (Port));
    Result : Natural := 0;
    procedure Fail is
    begin
        Ada.Command_Line.Set_Exit_Status (1);
    end Fail;
begin
    C.Connect;
    C.Increment ("incr_check", 1, Result);

    if Result /= 2 then
        Fail;
    end if;

    C.Disconnect;
end Incr_Check;
