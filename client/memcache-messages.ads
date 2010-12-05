--
--  Memcache.Messages specification
--
--  This spec should define all the supported messages we can send to
--  the memcached server running.
--

with Ada.Calendar, Ada.Strings.Bounded;

package Memcache.Messages is
    package Bounded is new Ada.Strings.Bounded.Generic_Bounded_Length(Max => 250);

    type Command is (Get, MultiGet, Set, Stats);
    type Key_Value is new Bounded.Bounded_String;
    type Message_Flags is mod 2 ** 16;

    type Message (C : Command) is record
        Key : Bounded.Bounded_String;

        case C is
            when Get | Set =>
                Flags    : Message_Flags;
                Expire   : Ada.Calendar.Time;
                Bytes    : Natural;
                No_Reply : Boolean := False; -- Optional
            when others =>
                null;
        end case;
    end record;

    function Serialize (Message_In : in Message) return String;

end Memcache.Messages;
