package IO is

   subtype Input is Character;
   type Output is mod 2 ** 16;

   procedure Open_Input (Fn : String);
   procedure Open_Output (Fn : String);
   procedure Get (X : out Input);
   procedure Put (X : Output);
   procedure Close_All;

end IO;
