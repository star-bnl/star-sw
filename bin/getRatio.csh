#! /usr/local/bin/tcsh -f
set ratio = \
`grep ' TPC hits:' $1 | awk '{print $7 $13}' | awk -F\: 'BEGIN {r=0 ; n = 0 ; }{t = $1 ; if ( t > 0 ) {a = $2 ; n++ ; r += a/t}} END {rr = r/n; printf("Ratio = %f\n",rr); }'`
