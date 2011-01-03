--
--  Test executable to create a connection, execute a `flush_all`
--  command and then disconnect.
--

with Ada.Command_Line;
with Ada.Text_IO;
with Memcache;

procedure Versioner is
    Port : String := Ada.Command_Line.Argument (1);
    C : Memcache.Connection :=
            Memcache.Create (Host => "127.0.0.1",
                            Port => Memcache.Port_Type'Value (Port));
begin
    C.Connect;
    Ada.Text_IO.Put_Line (C.Version);
    C.Disconnect;
end Versioner;
