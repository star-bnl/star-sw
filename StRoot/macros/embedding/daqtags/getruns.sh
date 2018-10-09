#!/bin/bash

list=`basename $1 .daq.list`.${2}.daq.list
rm -f $list

for i in `cat $1|unsort.pl|head -n $2`
do
   bname=`basename $i .daq`
   echo $i >> $list
   runnumber=`echo $bname | cut -d"_" -f4 -`
   echo $runnumber 
done
