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
jsonOut="$2"/dump.json
anderOut="$2"/ander.txt

# Remove old files
rm "$byteout" -f
rm "$jsonOut" -f
rm "$anderOut" -f

# TODO: Delete unused files instead of moving them
clang -S -emit-llvm "$1" -o "$byteout" -fno-discard-value-names
$SVF_BIN -nander "$byteout" -dump-json "$jsonOut" -write-ander "$anderOut" -dump-constraint-graph -dump-pag -dump-callgraph #-extapi="../SVF/node_modules/svf-lib/SVF-linux/Release-build/lib/extapi.bc"
mv consCG_initial.dot untitled/c-programs/
mv svfir_initial.dot untitled/c-programs/
mv callgraph_final.dot untitled/c-programs/
rm consCG_final.dot
rm callgraph_initial.dot
