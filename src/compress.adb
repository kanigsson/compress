with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Direct_IO;
with Ada.Text_IO;
with Ada.IO_Exceptions;
with Trie_List;

use type Ada.Containers.Count_Type;

package body Compress is

  
   package Decode_Maps is new Ada.Containers.Vectors
     (Index_Type => Natural,
      Element_Type => Unbounded_String);
   
   package Regular_IO is new Ada.Direct_IO (Input);
   package Compressed_IO is new Ada.Direct_IO (Output);
   

   T : Trie_List.Trie;
   
   Decode_Map : Decode_Maps.Vector;
   Counter : Output := 0;
   
   procedure Lookup 
     (S : String;
      Has_Entry : out Boolean;
      Key : in out Output) is
   begin
      if S = "" & ASCII.NUL then
         Has_Entry := True;
         Key := 0;
      else
         declare
            N : Natural := Trie_List.Find (T, S);
         begin
            if N = 0 then
               Has_Entry := False;
            else
               Has_Entry := True;
               Key := Output (N);
            end if;
         end;
      end if;
   end Lookup;
   
   procedure Lookup_Output (Key : Output;
                            Found : out Boolean;
                            S : out Unbounded_String)
   is
      use Decode_Maps;
   begin
      if Decode_Map.Length <= Ada.Containers.Count_Type (Key) then
         Found := False;
      else
         S := Decode_Map.Element (Natural (Key));
         Found := True;
      end if;
   end Lookup_Output;

   
   procedure Insert (S : String) is
   begin
      if Debug then
         Ada.Text_IO.Put_Line ("inserting " & S);
      end if;
      if S = "" & ASCII.NUL then
         null;
      else
         if Counter /= 0 then
            Trie_List.Insert (T, S, Positive (Counter));
         end if;
      end if;
      --  TODO what if all codes are used?
      Counter := Counter + 1;
   end Insert;
   
   procedure Insert_Decode (S : Unbounded_String) is
   begin
      Decode_Map.Append (S);
   end Insert_Decode;
   
   procedure Init_Map is
   begin
      for C in Input loop
         Insert ("" & C);
         Insert_Decode (Null_Unbounded_String & C);
      end loop;
   end Init_Map;

   procedure Compress (In_Fn, Out_Fn : String)
   is
      Buf          : String (1 .. 1000);
      Buf_Len      : Natural := 0;
      Key, New_Key : Output := 0;
      Has_Entry    : Boolean;
      Input_File   : Regular_IO.File_Type;
      Output_File  : Compressed_IO.File_Type;
   begin
      Regular_IO.Open (Input_File, Regular_IO.In_File, In_Fn);
      Compressed_IO.Create (Output_File, Compressed_IO.Out_File, Out_Fn);
      begin
         loop
            Regular_IO.Read (Input_File, Buf (Buf_Len + 1));
            Lookup (Buf (1 .. Buf_Len + 1), Has_Entry, New_Key);

            if Has_Entry then
               Buf_Len :=  Buf_Len + 1;
            else
               Insert (Buf (1 .. Buf_Len + 1));
               Compressed_IO.Write (Output_File, Key);
               Buf (1) := Buf (Buf_Len + 1);
               Buf_Len := 1;
            end if;
            Key := New_Key;
         end loop;
      exception
         when Ada.IO_Exceptions.End_Error =>
            null;
      end;
      --  account for remaining buffer
      Lookup (Buf (1 .. Buf_Len), Has_Entry, Key);
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
                  Insert_Decode (S & Element (T, 1));
               end if;
               S := T;
            else
               --  see Wikipedia entry on LZW
               --  https://en.wikipedia.org/wiki/Lempel%E2%80%93Ziv%E2%80%93Welch
               declare
                  U : Unbounded_String := S & Element (S, 1);
               begin
                  Insert_Decode (U);
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
