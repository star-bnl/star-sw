#!/bin/bash

totalsize=0
nfile=0

for i in `cat $1`
do
   bname=`basename $i .daq`
   echo $i 
   runnumber=`echo $bname | cut -d"_" -f4 -`
   echo $runnumber 
   filesize=`./getSize.pl -n $bname -t online_daq -s =hpss -g "runnumber=$runnumber" | awk '{print $2}'`
   nfile=$(($nfile+1))
   echo $filesize
   totalsize=$(($totalsize+$filesize))
   echo `echo $totalsize/1024/1024/1024 | bc -l | cut -d. -f1` "GB with $nfile files"
done

