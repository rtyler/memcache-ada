--
--  Simple test to run Dump_Stats
--

with Ada.Command_Line;
with Memcache;

procedure Stats is
    Port : String := Ada.Command_Line.Argument (1);
    C : Memcache.Connection :=
            Memcache.Create (Host => "127.0.0.1",
                            Port => Memcache.Port_Type'Value (Port));
begin
    C.Connect;
    C.Dump_Stats;
    C.Disconnect;
end Stats;
