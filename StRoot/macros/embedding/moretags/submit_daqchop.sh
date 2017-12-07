#!/bin/sh

if [ $# -ne 3 ] ; then
   echo ""
   echo " Usage : $0 [DaqList] [OutDir] [StarLibrary] "
   echo ""
   exit
fi

if [ ! -d sums ] ; then
   mkdir sums
fi
if [ ! -d logs ] ; then
   mkdir logs
fi
if [ ! -d output ] ; then
   echo 'FATAL Error!, ./output directory does not exist!'
   exit
fi
star-submit-template -template ./Scheduler_daqchop.xml -entities DaqList=`pwd`/$1,OutDir=$2,STARLib=$3
