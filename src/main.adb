with Ada.Command_Line;
with Ada.Text_IO;
with Compress;
with IO;

procedure Main is
begin
   if Ada.Command_Line.Argument_Count < 2 then
      Ada.Text_IO.Put_Line ("usage: compress <inputfile> <outputfile>");
   else
      IO.Open_Input (Ada.Command_Line.Argument (1));
      IO.Open_Output (Ada.Command_Line.Argument (2));
      Compress.Compress;
      IO.Close_All;
   end if;

end Main;
