#!/usr/bin/zsh

SVF_BIN="../SVF/node_modules/SVF/Release-build/bin/wpa"

if [ "$3" = "" ]
then
  echo "Please provide an input file and output file"
  exit 1
fi

clang -S -emit-llvm "$1" -o "$2" -fno-discard-value-names
$SVF_BIN -nander "$2" -dump-json "$3" -extapi="../SVF/node_modules/svf-lib/SVF-linux/Release-build/lib/extapi.bc"