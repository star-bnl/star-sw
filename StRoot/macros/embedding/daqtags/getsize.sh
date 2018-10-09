#!/bin/bash

if [[ ! $HOST =~ "rcas" ]] ; then
   echo this code can only be used for RCF!
   exit
fi

if [[ ! $# -eq 1 ]] ; then
   echo ""
   echo " Usage : $0 [good_daqfilelist.500.daq.list] "
   echo " output filelist : nevent_good_daqfilelist.500.daq.list. "
   echo ""
   exit
fi

totalsize=0
nfile=0
nevents=0

rm -f nevent_$1

for i in `cat $1`
do
   bname=`basename $i .daq`
   echo $i 
   runnumber=`echo $bname | cut -d"_" -f4 -`
   echo $runnumber 
   filesize=`./getSize.pl -n $bname -t online_daq -s =hpss -g "runnumber=$runnumber" | awk '{print $2}'`
   events=`./getSize.pl -n $bname -t online_daq -s =hpss -g "runnumber=$runnumber" | awk '{print $3}'`
   nfile=$(($nfile+1))
   echo $filesize
   totalsize=$(($totalsize+$filesize))
   nevents=$(($nevents+$events))
   echo `echo $totalsize/1024/1024/1024 | bc -l | cut -d. -f1` "GB with $nfile files"
   echo "$nevents events with $nfile files"
   echo ${bname}.daq $events >> nevent_$1
done

