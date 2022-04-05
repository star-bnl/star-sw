#!/bin/bash

if [[ ! $HOST =~ "rcas" ]] ; then
   echo this code can only be used for RCF!
   exit
fi

if [[ ! $# -eq 1 ]] ; then
   echo ""
   echo " Usage : $0 [daqfilelist.daq.list] "
   echo " list of good runs must be stored in goodruns.txt!"
   echo " alternatively, list of bad runs must be stored in badruns.txt!"
   echo " NOTE: goodruns.txt will override badruns.txt!"
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
   testgood="xxx"
   testbad=""
   if [ -f goodruns.txt ] ; then
	testgood=`grep $mystr goodruns.txt`
   elif [ -f badruns.txt ] ; then
	testbad=`grep $mystr badruns.txt`
   else
	echo "either goodruns.txt or badruns.txt should be provided!"
	exit
   fi
   if [[ ! -z "$testgood" && -z "$testbad" ]] ; then

	echo $i >> good_$1

   fi
done

