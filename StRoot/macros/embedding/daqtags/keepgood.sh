#!/bin/bash

if [[ ! $HOST =~ "rcas" ]] ; then
   echo this code can only be used for RCF!
   exit
fi

if [[ ! $# -eq 1 ]] ; then
   echo ""
   echo " Usage : $0 [daqfilelist.daq.list] "
   echo " list of good runs must be stored in goodruns.txt!"
   echo " output filelist : good_daqfilelist.daq.list. "
   echo ""
   exit
fi

rm -f good_$1

for i in `cat $1`
do
   bname=`basename $i`
   #echo $bname
   mystr=`echo $bname | cut -d"_" -f4 -`
   #echo $mystr
   teststr=`grep $mystr goodruns.txt`
   #echo $teststr
   if [ ! -z "$teststr" ] ; then

	echo $i >> good_$1

   fi
done

