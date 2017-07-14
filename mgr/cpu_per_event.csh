#! /bin/tcsh -f
set LOG = '*.log'
if ($#argv > 0)   set LOG = "$argv";
#echo $LOG
echo "$LOG" | grep 'gz$'
if ($? ) then
#    echo "use grep"
  grep 'Done with Event' $LOG | grep -v 'no. 1/' | sed -e 's/.*Cpu Time =//' |  awk  'BEGIN {n=0;j = 0;}{j++; n += $1;} END {print "CPU/event = "n/j "(sec) for "j" events"}'
else 
#    echo "use zgrep"
zgrep 'Done with Event' $LOG | grep -v 'no. 1/' | sed -e 's/.*Cpu Time =//' |  awk  'BEGIN {n=0;j = 0;}{j++; n += $1;} END {print "CPU/event = "n/j "(sec) for "j" events"}'
endif
unsetenv LOG

