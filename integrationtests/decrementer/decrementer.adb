--
--  Test executable to create a connection, execute a `delete`
--  command and then disconnect.
--

with Ada.Command_Line;
with Memcache;

procedure Decrementer is
    Port : String := Ada.Command_Line.Argument (1);
    C : Memcache.Connection :=
            Memcache.Create (Host => "127.0.0.1",
                            Port => Memcache.Port_Type'Value (Port));
    Result : Natural;
begin
    C.Connect;
    C.Decrement ("decrementer", 1, Result);
    C.Disconnect;

    if Result /= 9 then
        Ada.Command_Line.Set_Exit_Status (1);
    end if;
end Decrementer;
