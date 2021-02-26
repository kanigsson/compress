package Compress is

   subtype Input is Character;
   type Output is mod 2 ** 20;

   Debug : Boolean := False;

   procedure Init_Map;
   procedure Compress (In_Fn, Out_Fn : String);
   procedure Decompress (In_Fn, Out_Fn : String);

end Compress;
