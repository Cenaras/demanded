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


clang -S -emit-llvm "$1" -o "$byteout" -fno-discard-value-names
$SVF_BIN -nander "$byteout" -dump-json "$jsonOut" -write-ander "$anderOut" #-extapi="../SVF/node_modules/svf-lib/SVF-linux/Release-build/lib/extapi.bc"