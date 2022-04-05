#!/bin/bash

if [[ ! $HOST =~ "rcas" ]] ; then
   echo this code can only be used for RCF!
   exit
fi

if [[ $# -eq 0 || $# -gt 2 ]] ; then
   echo ""
   echo " Usage : $0 [good_xxx.500.Pxxiy.mudst.list]"
   echo "      or $0 [good_xxx.500.Pxxiy.mudst.list] [nevents]"
   echo "       [nevents] is the maximum # of events used in each file (default is 1000)"
   echo ""
   exit
fi

if [ $# -eq 1 ] ; then
   neventscut=1000
else
   neventscut=$2
fi

totall=0
tot=0
min=10000
max=0
ndaq=0
neffdaq=0
for i in `cat $1`
do
   bname=`basename $i .MuDst.root`
   #echo $bname
   mystr=`echo $bname`
   #echo $mystr
   teststr=`find output -name "${bname}*.chopper.txt"`
   teststr2=`find output -name "${bname}*.moretags.root"`
   if [[ ! -z "$teststr" && ! -z "$teststr2" ]] ; then
	neffdaq=$(($neffdaq+1))
   fi
   evts=`wc -l $teststr | awk '{print $1}' `
   ndaq=$(($ndaq+1))
   totall=$(($totall+$evts))
   if [ $evts -lt $min ] ; then
	min=$evts
   fi
   if [ $evts -gt $max ] ; then
	max=$evts
   fi
   if [ $evts -lt $neventscut ] ; then
	tot=$(($tot+$evts))
   else
	tot=$(($tot+$neventscut))
   fi
done

echo "# of daq/MuDst files (in the input file list): $ndaq"
echo "# of effective daq/MuDst files (with both .chopper.txt and .moretags.root produced in output/): $neffdaq"
echo "total effective events (passed event cuts): $totall"
echo "average # of effective events in each file: "$(($totall/$ndaq))
echo "the maximum # of effective events per file: $max"
echo "the minimum # of effective events per file: $min"
echo "assigned nevents cut value: $neventscut"
echo "total events per fset: $tot"

