#!/bin/bash

if [[ ! $HOST =~ "rcas" ]] ; then
   echo this code can only be used for RCF!
   exit
fi

if [[ ! $# -eq 2 ]] ; then
   echo ""
   echo " Usage : $0 [good_daqfilelist.500.daq.list] [nevents]"
   echo "       [nevents] is the maximum # of events used in each daq file"
   echo ""
   exit
fi

totall=0
tot=0
min=10000
max=0
ndaq=0
for i in `cat $1`
do
   bname=`basename $i .daq`
   #echo $bname
   mystr=`echo $bname`
   #echo $mystr
   teststr=`find output -name "${bname}*.chopper.txt"`
   evts=`wc -l $teststr | awk '{print $1}' `
   ndaq=$(($ndaq+1))
   totall=$(($totall+$evts))
   if [ $evts -lt $min ] ; then
	min=$evts
   fi
   if [ $evts -gt $max ] ; then
	max=$evts
   fi
   if [ $evts -lt $2 ] ; then
	tot=$(($tot+$evts))
   else
	tot=$(($tot+$2))
   fi
done

echo "# of daq files: $ndaq, total events in daq files: $totall, average # in one daq file: "$(($totall/$ndaq))", maximum # of events per file: $max, minimum # of events per file: $min, nevents cut value: $2, total events per fset: $tot"

