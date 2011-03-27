separate (Memcache)
    procedure Validate (Key : in String) is
    begin
        --  A key must be between 1 and 250 characters in length
        if Key'Length = 0  or Key'Length > 250 then
            raise Invalid_Key_Error;
        end if;

        --
        --  It's unfortunate that string scanning is apprently necessary,
        --  but we need to verify a few conditions for the string:
        --      * No whitespace
        --      * No control characters
        for Index in Key'Range loop
            declare
                Key_Ch : constant Character := Key (Index);
            begin
                if Character'Pos (Key_Ch) = 32 then
                    raise Invalid_Key_Error;
                end if;

                if Ada.Characters.Handling.Is_Control (Key_Ch) then
                    raise Invalid_Key_Error;
                end if;
            end;
        end loop;
    end Validate;


