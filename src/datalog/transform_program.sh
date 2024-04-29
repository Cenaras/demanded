#!/bin/bash

### Takes as input a variable, transform the exhaustive.dl to query that variable and performs magic sets transformation
### and stores the transformed program as demand.dl. Also fixes syntax errors and adds output directives for all
### intermediate computations.

### Arguments
### 1) Query Variable
### 2) Path to exhaustive.dl
### 3) Path to demand.dl

pathExhaustive=$2
pathDemand=$3

exhaustive="$(cat "$pathExhaustive" | head -n -1)"
echo "$exhaustive" > "$pathExhaustive"
echo "query(t) :- pointsTo(\"$1\", t)." >> "$pathExhaustive"

original="$(souffle --magic-transform=\* -F. -D. "$pathExhaustive" --show transformed-datalog)"
modified="$(echo "$original" | tr -d '@' | tr -d '{' | tr -d '}' | sed -E 's/([[:alpha:]]+)\.([[:alpha:]]+)/\1_\2/g')"
t="$(echo "$modified" | sed -E 's/([[:alpha:]]+)\.([[:alpha:]]+(\.[[:alpha:]]+)?)/\1_\2/g' | head -n -9)"
echo "$t" > "$pathDemand" 
echo ".input new" >> "$pathDemand"
echo ".input assign" >> "$pathDemand"
echo ".input load" >> "$pathDemand"
echo ".input store" >> "$pathDemand"
echo ".output query" >> "$pathDemand"
# Outputting all relations
echo ".output magic_pointsTo_bb" >> "$pathDemand"
echo ".output magic_pointsTo_bf" >> "$pathDemand"

echo ".output magic_pointsToField_bbb" >> "$pathDemand"
echo ".output magic_pointsToField_bbf" >> "$pathDemand"

echo ".output pointsTo_bb" >> "$pathDemand"
echo ".output pointsTo_bf" >> "$pathDemand"

if [[ "$pathExhaustive" == *exhaustive1.dl ]]; then
  echo ".output pointsTo_fb" >> "$pathDemand"
  echo ".output magic_pointsTo_fb" >> "$pathDemand"
fi

echo ".output pointsToField_bbb" >> "$pathDemand"
echo ".output pointsToField_bbf" >> "$pathDemand"
