#! /usr/local/bin/tcsh -f
 ls -1 0*.root | awk -F_ '{print $3}' | awk -F. 'BEGIN {n=0}{n+= $1} END {print "Total = "n}'
