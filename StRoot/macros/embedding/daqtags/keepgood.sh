#!/bin/bash

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

