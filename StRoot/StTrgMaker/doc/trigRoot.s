#!/usr/bin/ksh
# AUTHOR:   Herbert Ward, May 2001.
# INTENDED USERS: trigger personnel not familiar with root4star.
# PURPOSE:  Turn-key CTB calibration.
# INPUT: A set of *.event.root files (typically all from one run).
# OUTPUT: A .ps file for ghostviewing, archiving, and/or printing.
# WHICH COMPUTER: Run this script on a Solaris machine which has the
#                 STAR offline environment (eg, rmine602.rhic.bnl.gov).
# HOW IT WORKS: For each input file, root4star runs and generates an
#               ASCII summary file in the temp dir jjTempDir.  After these
#               summary files are created, they are combinined into one
#               big file and the program $OVERALL is run to create a .plot
#               file.  The program trigPlot ($TRIGPLOT) is run on the
#               .plot file to produce a .ps (PostScript) file, which is
#               the final output.
# HOW TO MONITOR PROGRESS: In a separate window, type "ls jjTempDir".
# IN CASE OF TROUBLE: Find Lidia.  Check that your value of $STAR is correct
#                     for the input files.  Check that you have the offline
#                     STAR setup (eg, type "which root4star").
################################# The "watch progress mode".
# This needs to be 
# first in the script so that this special mode doesn't clobber the output
# directory or other files.
if test $# -ge 1
then
  if test $1 = watchProgress
  then
    while true
    do
      echo ooooooooooooooooooooooooooooooooooooooo
      ls -l jjTempDir
      date
      echo Use control-C to stop this.
      sleep 15
    done
  fi
fi
################################# Set up variables for later use.
STARTTIME=`date`
OVERALL=progs/trk2slat/overall
TRIGPLOT=progs/trigPlot/trigPlot
################################# Check usage.
if test $# -lt 1
then
  echo "Usage:  " $0 listOfEventRootFiles
  echo "Example:" $0 /xxx/yyy/zzz/\*_1201003_\*.event.root
  echo or
  echo "Usage:  " $0 watchProgress
  exit
fi
################################# Clean up the temp directory.
echo Cleaning up the temp directory.
rm -r jjTempDir 2> /dev/null
mkdir jjTempDir
################################# Prompt user for the title.
echo XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
echo Hello.  Please type a one-word title for output plot \(for
echo example, the run number\):
read temp
title=`echo $temp | sed 's/ .*//'`
echo The title is \"$title\".
sleep 3
################################# Blurb.
echo XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
echo If you want to watch the progress, get a separate window,
echo and type
echo "            " cd `pwd`
echo "            " $0 watchProgress
echo Press return to continue.
read junk
################################# Check for necessary aux files.
echo Checking for necessary auxiliary files.
for ii in trigCtb.C $OVERALL $TRIGPLOT
do
  if test -r $ii
  then
    echo OK, file exists: $ii
  else
    echo FATAL ERROR, file does not exist: $ii
    exit
  fi
done
################################# Run root4star, looping over the input files.
while true
do
  if test $# -lt 1
  then
    break
  fi
  currentFile=$1
  shift
  rm StTrgMaker.out 2> /dev/null
  root4star -b -q 'trigCtb.C(9999999,"'$currentFile'")'
  if test -r StTrgMaker.out
  then
    echo XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    echo Finished root4star, for $currentFile.
    echo $currentFile contains `grep '^e' StTrgMaker.out | wc -l` events
    echo with primary vertex passing the cuts.
  else
    echo Fatal error, root4star did not make StTrgMaker.out.  Aborting.
    exit
  fi
  outname=`echo $currentFile | sed 's-^.*/--' | sed 's/\.root$/\.OUT/`
  mv StTrgMaker.out jjTempDir/$outname
done
################################# Cat the output files together and run app.
cat jjTempDir/*.OUT > jjTempDir/total.out
rm total.plot 2> /dev/null
$OVERALL jjTempDir/total.out $title
mv total.plot jjTempDir
################################# Run trigPlot
$TRIGPLOT -bi jjTempDir/total.plot
mv -i jjTempDir/total.ps $title.ps
################################# Final messages to user.
echo "XXXXXXXX  FINAL REPORT  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
echo Your output is named $title.ps
echo You can print it, archive it, or ghostview it:
echo ghostview $title.ps
ENDTIME=`date`
echo "started   at " $STARTTIME
echo "ended     at " $ENDTIME
