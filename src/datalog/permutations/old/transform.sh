#!/bin/sh

# Transforms a exhaustive analysis into a magic sets transformed equivalent one. Strips away non-relevant data
# Note that the result is not supposed to be compilable! It just expresses the analysis rules!

pathExhaustive="untitled/src/datalog/permutations/analysis.dl"
pathOutput="untitled/src/datalog/permutations/output.dl"

echo "Calling with $pathExhaustive and $pathOutput"

original="$(souffle --magic-transform=\* -F. -D. "$pathExhaustive" --show transformed-datalog)"
echo "$original"

modified="$(echo "$original" | tr -d @ | tr -d '{' | tr -d '}' | sed -E 's/([[:alpha:]]+)\.([[:alpha:]]+)/\1_\2/g' )"
t="$(echo "$modified" | sed -E 's/([[:alpha:]]+)\.([[:alpha:]]+(\.[[:alpha:]]+)?)/\1_\2/g' | head -n -9 )"
echo "$t" > "$pathOutput"

#echo "$t"
echo "Finished calling script"