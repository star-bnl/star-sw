#! /bin/tcsh -f
set LOG = '*.log'
if ($#argv > 0)   set LOG = "$argv";
#grep \.gz '$LOG'
#if ($? ) then
  grep 'Done with Event' $LOG | grep -v 'no. 1/' | sed -e 's/.*Cpu Time =//' |  awk  'BEGIN {n=0;j = 0;}{j++; n += $1;} END {print "CPU/event = "n/j "(sec) for "j" events"}'
#else 
#zcat ${LOG} |  grep 'Done with Event' | grep -v 'no. 1/' | sed -e 's/.*Cpu Time =//' |  awk  'BEGIN {n=0;j = 0;}{j++; n += $1;} END {print "CPU/event = "n/j "(sec) for "j" events"}'
#endif
unsetenv LOG

