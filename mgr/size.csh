#! /usr/local/bin/tcsh -f
set grand_size = \
`awk  'BEGIN {n=0;j = 0;}{j++; n += $5;} END {print "Total = "n/1.e9"(GB) in "j" files"}' b*.list`
set grand_dst  = \
`awk  'BEGIN {n=0;j = 0;}{j++; n += $5;} END {print "Total = "n/1.e9"(GB) in "j" files"}' tfs*.list`
echo "Grand size: Input " ${grand_size} " -> Output " ${grand_dst} 
