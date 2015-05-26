grep 'NFitP>=15' $1 | awk 'BEGIN {n=0;j = 0;}{j++; if ($12>=2) n++} END {print n" "j " " 100.*n/j}'
