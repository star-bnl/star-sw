#
if (! -e ./condor ) mkdir condor 
#	cleanup directory
set x = (./condor/*.tags.root)
if (-e $x[1]) rm -f $x

# 	How many files
set x = (`cat $STV_FILE_LIST | grep -v '#' | wc `)
set nJobs = $x[1]

# 	create .xml
cop runStv.xml.src runStv.xml
modify _STVOPTS_ "$STV_OPTS" 		runStv.xml
modify _STVJOBS_ $nJobs    		runStv.xml
modify _STVFILELIST_ "$STV_FILE_LIST"	runStv.xml

star-submit-beta runStv.xml

@ mins = 0
AGAIN: 
  sleep 600
  @ mins = $mins + 10
  echo "runCondor WAITING $mins minutes"
  set x = (`condor_q $user -long | grep Cmd | grep $cwd | wc `)
  if ($x[1] ) goto AGAIN

echo "runCondor ENDED $mins minutes"

set x = (./condor/*.tags.root)
if ($x[1] != $nJobs) sleep 600
set x = (./condor/*.tags.root)
set nJobsEnded = $#x;
if ($nJobsEnded != $nJobs) goto FAILED
exit 0


FAILED:
 echo "runCondor FAILED jobs $nJobs ended $nJobsEnded"
exit 13

