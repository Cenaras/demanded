#!/bin/sh

# Iterates the analyses directory, and performs magic sets transformation for each program
# Also iterates the output directory and removes any duplicate files

inputDir="untitled/src/datalog/permutations/analyses"
outputDir="untitled/src/datalog/permutations/transformations"

for file in "$inputDir"/*; do
  file_name=$(basename "$file")
  name="${file_name%.*}"

  original="$(souffle --magic-transform=\* -F. -D. "$file" --show transformed-datalog)"
  modified="$(echo "$original" | tr -d @ | tr -d '{' | tr -d '}' | sed -E 's/([[:alpha:]]+)\.([[:alpha:]]+)/\1_\2/g' )"
  t="$(echo "$modified" | sed -E 's/([[:alpha:]]+)\.([[:alpha:]]+(\.[[:alpha:]]+)?)/\1_\2/g' | head -n -9 )"

  echo "$t" > "$outputDir/$name.dl"
done

checksum_file=$(mktemp)
files_with_all_free=$(mktemp)
demanded_files=$(mktemp)

for file in "$outputDir"/*; do
  # Extract only sha256, discard filename
  sum=$(sha256sum "$file" | awk '{print $1}')

  if grep -q "$sum" "$checksum_file"; then
    echo "Removing duplicate file $file"
    rm "$file"
  else
    echo "Adding new sum $sum for analysis $file"
    echo "$sum" > "$checksum_file"
  fi

  if grep -qE '\.decl [a-zA-Z0-9_]+_ff\b' "$file" || grep -qE '\.decl [a-zA-Z0-9_]+_fff\b' "$file"; then
    echo "$file" >> "$files_with_all_free"
  else
    echo "$file" >> "$demanded_files"
  fi


done

sort -u "$files_with_all_free" -o "$files_with_all_free"
sort -u "$demanded_files" -o "$demanded_files"

num_entries=$(wc -l < "$files_with_all_free")
echo "Files with exhaustive pattern: $num_entries"

echo "Pure demanded files:"
cat "$demanded_files"

rm "$checksum_file"
rm "$files_with_all_free"

