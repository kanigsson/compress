with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Direct_IO;
with Ada.Text_IO;

package body Compress is

   package My_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => Output,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => "=");
   
   package Regular_IO is new Ada.Direct_IO (Input);
   package Compressed_IO is new Ada.Direct_IO (Output);
   
  
   Map : My_Maps.Map;
   Counter : Output := 0;
   
   procedure Lookup 
     (S : Unbounded_String;
      Has_Entry : out Boolean;
      Key : in out Output) is
   begin
      if Map.Contains (S) then 
         Has_Entry := True;
         Key := Map.Element (S);
      else
         Has_Entry := False;
      end if;
   end Lookup;
   
   procedure Lookup_Output (Key : Output;
                            Found : out Boolean;
                            S : out Unbounded_String)
   is
      use My_Maps;
      C : Cursor := First (Map);
   begin
      while Has_Element (C) loop
         if Element (C) = Key then
            Found := True;
            S := My_Maps.Key (C);
            return;
         end if;
         Next (C);
      end loop;
      Found := False;
      S := Null_Unbounded_String;
   end Lookup_Output;

   
   procedure Insert (S : Unbounded_String) is
   begin
      if Debug then
         Ada.Text_IO.Put_Line ("inserting " & To_String (S));
      end if;
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

   procedure Compress (In_Fn, Out_Fn : String)
   is
      S           : Unbounded_String;
      C           : Character;
      Key         : Output := 0;
      Has_Entry   : Boolean;
      Input_File  : Regular_IO.File_Type;
      Output_File : Compressed_IO.File_Type;
   begin
      Regular_IO.Open (Input_File, Regular_IO.In_File, In_Fn);
      Compressed_IO.Create (Output_File, Compressed_IO.Out_File, Out_Fn);
      while not Regular_IO.End_Of_File (Input_File) loop
         Regular_IO.Read (Input_File, C);
         Lookup (S & C, Has_Entry, Key);

         if Has_Entry then
            S := S & C;
         else
            Insert (S & C);
            Lookup (S, Has_Entry, Key);
            pragma Assert (Has_Entry);
            Compressed_IO.Write (Output_File, Key);
            S := Null_Unbounded_String & C;
         end if;
      end loop;
      --  account for last character
      Lookup (S, Has_Entry, Key);
      pragma Assert (Has_Entry);
      Compressed_IO.Write (Output_File, Key);
   end Compress;
   
   procedure Decompress (In_Fn, Out_Fn : String) is
      S           : Unbounded_String;
      Key         : Output := 0;
      Has_Entry   : Boolean;
      Input_File  : Compressed_IO.File_Type;
      Output_File : Regular_IO.File_Type;
   begin
      Compressed_IO.Open (Input_File, Compressed_IO.In_File, In_Fn);
      Regular_IO.Create (Output_File, Regular_IO.Out_File, Out_Fn);      
      while not Compressed_IO.End_Of_File (Input_File) loop
         Compressed_IO.Read (Input_File, Key);
         declare
            T : Unbounded_String;
         begin
            Lookup_Output (Key, Has_Entry, T);
            if Has_Entry then
               if Debug then
                  Ada.Text_IO.Put_Line ("decoding to " & To_String (T));
               end if;
               for J in 1 .. Length (T) loop
                  Regular_IO.Write (Output_File, Element (T, J));
               end loop;
               --  This test is there to skip inserting after the first code.
               --  Test should be true only in the first iteration.
               if S /= Null_Unbounded_String then
                  Insert (S & Element (T, 1));
               end if;
               S := T;
            else
               --  see Wikipedia entry on LZW
               --  https://en.wikipedia.org/wiki/Lempel%E2%80%93Ziv%E2%80%93Welch
               declare
                  U : Unbounded_String := S & Element (S, 1);
               begin
                  Insert (U);
                  S := U;
                  for J in 1 .. Length (U) loop
                     Regular_IO.Write (Output_File, Element (U, J));
                  end loop;
               end;
            end if;
         end;
      end loop;
   end Decompress;
      
end Compress;
