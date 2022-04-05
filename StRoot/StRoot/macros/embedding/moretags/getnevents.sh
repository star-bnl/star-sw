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

nfile=0
nevents=0

rm -f nevent_$1

for i in `cat $1`
do
   bname=`basename $i .daq`
   echo $i 
   events=`wc -l output/${bname}.chopper.txt | awk '{print $1}'`
   nfile=$(($nfile+1))
   nevents=$(($nevents+$events))
   echo "$nevents events with $nfile files"
   echo ${bname}.daq $events >> nevent_$1
done

