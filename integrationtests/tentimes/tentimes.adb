--
--  Test executable to create a connection, execute a `delete`
--  command and then disconnect.
--

with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Memcache;

use Ada.Strings.Unbounded;
use Ada.Text_IO;

procedure Tentimes is
    package SF renames Ada.Strings.Fixed;
    package SU renames Ada.Strings.Unbounded;

    Port : String := Ada.Command_Line.Argument (1);
    C : Memcache.Connection :=
            Memcache.Create (Host => "127.0.0.1",
                            Port => Memcache.Port_Type'Value (Port));
    procedure Fail is
    begin
        Ada.Command_Line.Set_Exit_Status (1);
    end Fail;
begin
    C.Connect;
    for Index in Integer range 1 .. 10 loop
        declare
            SIndex : constant String := SF.Trim (Integer'Image (Index), Ada.Strings.Both);
            RC : Boolean := False;
        begin
            Put_Line ("Setting a key/value pair: " & SIndex & "/" & SIndex);
            RC := C.Set (SIndex, SIndex);

            if RC /= True then
                Put_Line ("Somehow failed to `set` the key: `" & SIndex & "`");
                Fail;
                exit;
            end if;

            Put_Line ("Retrieving `" & SIndex & "`...");
            declare
                R : constant Memcache.Response := C.Get (SIndex);
                Value : constant String := SU.To_String (R.Data);
            begin
                Put_Line ("Got a value for key: `" & SIndex &
                                        "` --> `" & Value & "`");
                if Value /= SIndex then
                    Put_Line ("Retrieved value mismatch");
                    Fail;
                    exit;
                end if;
            end;
        end;
    end loop;
    C.Disconnect;
end Tentimes;
