#! /bin/tcsh -f
set LOG = '*.log'
if ($#argv > 0)   set LOG = "$argv";
grep 'Done with Event' $LOG | grep -v 'no. 1/' | awk  'BEGIN {n=0;j = 0;}{j++; n += $17;} END {print "CPU/event = "n/j "(sec) for "j" events"}'
unsetenv LOG

