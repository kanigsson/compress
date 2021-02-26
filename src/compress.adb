with Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Vectors;
with Ada.Direct_IO;
with Ada.Text_IO;
with Ada.IO_Exceptions;
with Ada.Containers.Indefinite_Ordered_Maps;
with Trie;

use type Ada.Containers.Count_Type;
use type Trie.Byte;
use type Trie.Byte_String;

package body Compress is

  
   package Decode_Maps is new Ada.Containers.Indefinite_Vectors
     (Index_Type => Natural,
      Element_Type => Trie.Byte_String);
   
   package Regular_IO is new Ada.Direct_IO (Trie.Byte);
   package Compressed_IO is new Ada.Direct_IO (Output);
   
   function Byte_String_To_String (S : Trie.Byte_String) return String is
      use Ada.Strings.Unbounded;
      Buf : Unbounded_String;
   begin
      for C of S loop
         Buf := Buf & C'Img;
      end loop;
      return """" & To_String (Buf) & """";
   end Byte_String_To_String;

   package String_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Key_Type => Trie.Byte_String,
      Element_Type => Output,
      "<" => "<",
      "=" => "=");
      
   Code_Map : String_Maps.Map;
   T : Trie.Trie;
   
   Decode_Map : Decode_Maps.Vector;
   Counter : Output := 0;
   
   procedure Lookup 
     (S : Trie.Byte_String;
      Has_Entry : out Boolean;
      Key : out Output)
   is
      use String_Maps;
      C : Cursor := Code_Map.Find (S);
   begin
      if Has_Element (C) then
         Has_Entry := True;
         Key := Element (C);
      else
         Has_Entry := False;
      end if;
   end Lookup;
   
   function Lookup_Output (Key : Output;
                           Found : out Boolean)
     return Trie.Byte_String
   is
      use Decode_Maps;
   begin
      if Decode_Map.Length <= Ada.Containers.Count_Type (Key) then
         Found := False;
         return (1 .. 0 => <>);
      else
         Found := True;
         return Decode_Map.Element (Natural (Key));
      end if;
   end Lookup_Output;

   
   procedure Insert (S : Trie.Byte_String) is
   begin
      Code_Map.Insert (S, Counter);
      Counter := Counter + 1;
   end Insert;
   
   procedure Insert_Decode (S : Trie.Byte_String) is
   begin
      Decode_Map.Append (S);
   end Insert_Decode;
   
   procedure Init_Map is
   begin
      for C in Trie.Byte loop
         Insert ((1 => C));
         Insert_Decode ((1 => C));
      end loop;
   end Init_Map;

   procedure Compress (In_Fn, Out_Fn : String)
   is
      Buf          : Trie.Byte_String (1 .. 1000);
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
               Lookup (Buf (1 .. 1), Has_Entry, New_Key);
               pragma Assert (Has_Entry);
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
      Buf         : Trie.Byte_String (1 .. 1000) := (1 .. 1000 => 0);
      Buf_Len     : Natural := 0;
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
            T : Trie.Byte_String := Lookup_Output (Key, Has_Entry);
         begin
            if Has_Entry then
               for J in 1 .. T'Length loop
                  Regular_IO.Write (Output_File, T (J));
               end loop;
               --  This test is there to skip inserting after the first code.
               --  Test should be true only in the first iteration.
               if Buf_Len /= 0 then
                  Insert_Decode (Buf (1 .. Buf_Len) & T (1));
               end if;
               Buf (1 .. T'Length) := T;
               Buf_Len := T'Length;
            else
               --  see Wikipedia entry on LZW
               --  https://en.wikipedia.org/wiki/Lempel%E2%80%93Ziv%E2%80%93Welch

               Buf_Len := Buf_Len + 1;
               Buf (Buf_Len) := Buf (1);
               Insert_Decode (Buf (1 .. Buf_Len));
               for J in 1 .. Buf_Len loop
                  Regular_IO.Write (Output_File, Buf (J));
               end loop;
            end if;
         end;
      end loop;
   end Decompress;
      
end Compress;
