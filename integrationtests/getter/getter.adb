--

with Ada.Command_Line;
with Memcache;

procedure Getter is
    Port : String := Ada.Command_Line.Argument (1);
    C : Memcache.Connection :=
            Memcache.Create (Host => "127.0.0.1",
                            Port => Memcache.Port_Type'Value (Port));
begin
    C.Connect;
    declare
        Result : String := C.Get("getter");
    begin
        if Result = "derpderp" then
            Ada.Command_Line.Set_Exit_Status(0);
        else
            Ada.Command_Line.Set_Exit_Status(1);
        end if;
    end;
    C.Disconnect;
end Getter;
