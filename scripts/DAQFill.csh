#!/bin/csh

#
# Quick wrapper to the perl script which
# function as an infinit loop process. Safer
# this way because the cron stuff have the tendancies
# to die ...
# Will be running on some node ...
#
# % DAQFill.csh {Update|Clean|Run|Purge} [SleepTime]
#
#
setenv COLUMNS 512
set PATH="/afs/rhic.bnl.gov/star/packages/scripts"
set SCRIPT="DAQFill.pl"
set LOG="$HOME/DAQFill.log"
set ME=`whoami`

unalias cd
unset noclobber


# get the sleep time as second argument
if ("$2" != "") then
    set SLTIME=$2
else
    set SLTIME=60
endif


# first argument is the primary option
if ( "$1" == "Clean") then
  # since AFS fluke may induce strange effects
  # auwx is NOT Unix-universal. Use it on Linux.
  set TEST=`/bin/ps auwx | /bin/grep $ME | /bin/grep $SCRIPT | /bin/grep -v grep | /bin/awk '{print $2}' | /usr/bin/xargs`
  if ("$TEST" != "") then
    echo "Killing $TEST on `date`"
    kill -9 $TEST
  endif

else if ( "$1" == "Insert") then
  # Run the script in Insert mode i.e. fetch intermediate records
  # we may have missed. Sleep time is irrelevant in this mode.
  cd $PATH
  ./$SCRIPT 0 $SLTIME

else if ( "$1" == "Update") then
  # This mode will bootstrap the entries entered since some
  # period of time and remove the ones which have been marked
  # improperly evaluated. This happens from time to time (meaning
  # the improper entries) and the cause is null return value
  # from the SQL querry.  
  cd $PATH
  ./$SCRIPT -1 $SLTIME

else
  # default option is to Run
  set TEST=`/bin/ps -ef | /bin/grep $ME | /bin/grep $SCRIPT | /bin/grep -v grep`
  if ("$TEST" == "") then
    cd $PATH
    if( ! -e $LOG) then
    	echo "Starting on `date`" >$LOG
    endif
    echo "Executing ./$SCRIPT 1 $SLTIME $LOG on `date`"
    ./$SCRIPT 1 $SLTIME $LOG &
  endif

endif





