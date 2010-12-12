--
--  Lulz

with Ada.Text_IO;
with Memcache;

procedure Deleter is
    C : Memcache.Connection :=
            Memcache.Create (Host => "127.0.0.1",
                            Port => 11211);

begin
    Ada.Text_IO.Put_Line ("Derp");
end Deleter;
