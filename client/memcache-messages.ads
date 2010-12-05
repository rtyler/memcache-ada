--
--  Memcache.Messages specification
--
--  This spec should define all the supported messages we can send to
--  the memcached server running.
--

with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

package Memcache.Messages is
    type Message is limited private;

    Stats : constant Message;

    function Serialize(Message_In : in Message) return String;

private
    type Message is record
        Raw_Command : Unbounded_String;
    end record;

    Stats : constant Message := Message'(Raw_Command => To_Unbounded_String("stats\r\n"));

end Memcache.Messages;
