#! /usr/local/bin/tcsh -f
set grand_size = \ 
`grep 'StTpcHitMaker:INFO  -  Total hits in Sector : 15' *B.log | awk  'BEGIN {n=0;j = 0;}{j++; n += $10;} END {print "Total = n in j events"}'`
echo "Grand size:" ${grand_size} 
