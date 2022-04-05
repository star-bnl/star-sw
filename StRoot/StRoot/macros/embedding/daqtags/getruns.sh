#!/bin/bash

if [[ ! $HOST =~ "rcas" ]] ; then
   echo this code can only be used for RCF!
   exit
fi

if [[ ! $# -eq 2 ]] ; then
   echo ""
   echo " Usage : $0 [good_daqfilelist.daq.list] [#files] | sort | uniq | wc -l "
   echo " output filelist : good_daqfilelist.[#files].daq.list. "
   echo ""
   exit
fi

list=`basename $1 .daq.list`.${2}.daq.list
rm -f $list

for i in `cat $1|unsort.pl|head -n $2`
do
   bname=`basename $i .daq`
   echo $i >> $list
   runnumber=`echo $bname | cut -d"_" -f4 -`
   echo $runnumber 
done
