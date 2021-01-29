with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Containers.Indefinite_Hashed_Maps;
with IO; use IO;

package body Compress is

   package My_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => Output,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => "=");
   
   Map : My_Maps.Map;
   Counter : Output := 0;
   
   --  ??? using Output'Last is incorrect ...
   function Lookup (S : Unbounded_String) return Output is 
     (if Map.Contains (S) then Map.Element (S) else Output'Last);
   
   procedure Insert (S : Unbounded_String) is
   begin
      pragma Assert (not Map.Contains (S));
      Map.Insert (S, Counter);
      Counter := Counter + 1;
   end Insert;

   procedure Compress is
      S : Unbounded_String;
      C : Character;
   begin
      while True loop
         Get (C);
         declare
            Key : Output := Lookup (S & C);
         begin
            --  ??? incorrect to use Output'Last
            
            if Key /= Output'Last then
               S := S & C;
            else
               Insert (S & C);
               Put (Key);
               S := Null_Unbounded_String;
            end if;
         end;
      end loop;
   end Compress;

end Compress;
