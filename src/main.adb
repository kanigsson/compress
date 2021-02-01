with Ada.Command_Line;
with Ada.Text_IO;
with Compress;

procedure Main is

   -----------
   -- Usage --
   -----------

   procedure Usage is
   begin
      Ada.Text_IO.Put_Line ("usage: compress [-d] <inputfile> <outputfile>");
   end Usage;

begin
   Compress.Init_Map;
   Compress.Debug := False;
   if Ada.Command_Line.Argument_Count < 2 then
      Usage;
   elsif Ada.Command_Line.Argument (1) = "-d"
     and then Ada.Command_Line.Argument_Count < 3
   then
      Usage;
   elsif Ada.Command_Line.Argument (1) = "-d" then
      Compress.Decompress (Ada.Command_Line.Argument (2),
                           Ada.Command_Line.Argument (3));
   else
      Compress.Compress (Ada.Command_Line.Argument (1),
                           Ada.Command_Line.Argument (2));
   end if;

end Main;
