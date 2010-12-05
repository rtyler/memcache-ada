--
--  Memcache.Messages specification
--
--  This spec should define all the supported messages we can send to
--  the memcached server running.
--

with Ada.Calendar;

package Memcache.Messages is

   type Command is (Get, Set, Stats);

   type Key_Value is new String (1 .. 250);

   type Message_Flags is mod 2 ** 16;

   type Message (C : Command) is record
      Key : Key_Value;

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
