### Takes as input a variable, transform the exhaustive.dl to query that variable and performs magic sets transformation
### and stores the transformed program as demand.dl. Also fixes syntax errors and adds output directives for all
### intermediate computations.

exhaustive="$(cat exhaustive.dl | head -n -1)"
echo "$exhaustive" > exhaustive.dl
echo "query(t) :- pointsTo(\"$1\", t)." >> exhaustive.dl

original="$(souffle --magic-transform=\* -F. -D. exhaustive.dl --show transformed-datalog)"
modified="$(echo "$original" | tr -d '@' | tr -d '{' | tr -d '}' | sed -E 's/([[:alpha:]]+)\.([[:alpha:]]+)/\1_\2/g')"
t="$(echo "$modified" | sed -E 's/([[:alpha:]]+)\.([[:alpha:]]+(\.[[:alpha:]]+)?)/\1_\2/g' | head -n -9)"
echo "$t" > demand.dl
echo ".input new" >> demand.dl
echo ".input assign" >> demand.dl
echo ".input load" >> demand.dl
echo ".input store" >> demand.dl
echo ".output query" >> demand.dl
# Outputting all relations
echo ".output magic_pointsTo_bb" >> demand.dl
echo ".output magic_pointsTo_bf" >> demand.dl

echo ".output magic_pointsToField_bbb" >> demand.dl
echo ".output magic_pointsToField_bbf" >> demand.dl

echo ".output pointsTo_bb" >> demand.dl
echo ".output pointsTo_bf" >> demand.dl

echo ".output pointsToField_bbb" >> demand.dl
echo ".output pointsToField_bbf" >> demand.dl

# TODO: We still need to transform the query and and magic fact generation for this to work properly.
#   We must be able to control that from the Scala code, since that is what's going to construct the query.
#   I suggest just passing it as argument to this file.