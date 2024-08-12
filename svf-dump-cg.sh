#!/usr/bin/zsh

SVF_BIN="../SVF/Release-build/bin/wpa"
# TODO: Use the binary from ./build.sh instead
if [ "$2" = "" ]
then
  echo "Please provide an input file and output directory"
  exit 1
fi

basename=$(basename "$1")
filename="${basename%.*}"

byteout="$2"/"$filename".bc
anderOut="$2"/ander.txt
outfile="$2"/output.txt

# Remove old files
rm "$byteout" -f
rm "$anderOut" -f


clang -S -emit-llvm "$1" -o "$byteout" -fno-discard-value-names
$SVF_BIN -nander "$byteout" -dump-constraint-graph -write-ander "$anderOut"
mv consCG_final.dot consCG_initial.dot untitled/c-programs/