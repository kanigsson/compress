with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Containers.Indefinite_Hashed_Maps;
with IO; use IO;
with Ada.Text_IO;

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
   
   procedure Init_Map is
   begin
      for C in Input loop
         Insert (Null_Unbounded_String & C);
      end loop;
   end Init_Map;

   procedure Compress is
      S : Unbounded_String;
      C : Character;
      Key : Output := 0;
   begin
      while not IO.Input_Exhausted loop
         Get (C);
         Ada.Text_IO.Put_Line ("input is " & C);
         Key := Lookup (S & C);

         --  ??? incorrect to use Output'Last
            
         if Key /= Output'Last then
            S := S & C;
         else
            declare
               Op : constant Output := Lookup (S);
            begin
               Ada.Text_IO.Put_Line 
                 ("found entry " & To_String (S) & ", output " & Op'Img
                  & ", creating new entry for " & To_String (S & C));
               Insert (S & C);
               Put (Op);
            end;

            S := Null_Unbounded_String;
         end if;
      end loop;
   end Compress;

end Compress;
