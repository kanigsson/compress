cd $1
echo "compressing ..."
../../obj/main source.txt compressed.txt
echo "decompressing ..."
../../obj/main -d compressed.txt decompressed.txt
echo "checking for diffs ..."
diff -q source.txt decompressed.txt
ls -l *txt
