#! /bin/tcsh -f
set LOG = '*.log'
if ($#argv > 0)   set LOG = "$argv";
#echo $LOG
set GREP = "grep";
echo "$LOG" | grep 'gz$'
if (! $? ) set GREP = "zgrep"
#    echo "use grep"
${GREP} 'Done with Event' $LOG | grep -v 'no. 1/' | sed -e 's/.*Real Time =//' |  awk  'BEGIN {n=0;j = 0; k= 0;}{j++; k += $1; n += $6} END {print "\tCPU/event = "n/j "(sec) and \tTotal/event = "k/j "(sec)  \t"100*n/k" % \tfor "j" events"}'
unset LOG
unset GREP
