with Ada.Direct_IO;

package body IO is

   package Input_IO is new Ada.Direct_IO (Input);
   package Output_IO is new Ada.Direct_IO (Output);

   In_Fs : Input_IO.File_Type;
   Out_Fs : Output_IO.File_Type;

   procedure Open_Input (Fn : String) is
   begin
      Input_IO.Open (In_FS, Input_IO.In_File, Fn);
   end Open_Input;

   procedure Open_Output (Fn : String) is
   begin
      Output_IO.Create (Out_Fs, Output_IO.Out_File, Fn);
   end Open_Output;

   procedure Get (X : out Input) is
   begin
      Input_IO.Read (In_FS, X);
   end Get;

   procedure Put (X : Output) is
   begin
      Output_IO.Write (Out_FS, X);
   end Put;

   procedure Close_All is
   begin
      Input_IO.Close (In_FS);
      Output_IO.Close (Out_Fs);
   end Close_All;

end IO;
