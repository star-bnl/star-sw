#! /usr/local/bin/tcsh -f
awk  'BEGIN {n=0;j = 0;}{j++; n += $5;} END {print "Total = "n/1.e9"(GB) in "j" files"}' $1

