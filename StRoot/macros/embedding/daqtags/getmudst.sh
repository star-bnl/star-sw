#!/bin/bash

daqlist=$1
prod=$2

lbn=`basename $daqlist .daq.list`

rm -f ${lbn}.${prod}.mudst.list
rm -f ${lbn}.${prod}.tags.list

for i in `cat $daqlist`
do
   bname=`basename $i .daq`
   echo $bname
   runnumber=`echo $bname | cut -d"_" -f4 -`
   echo $runnumber 
   mdstfile=`./getFiles.pl -p ${prod} -n $bname -t daq_reco_MuDst -s =hpss -g "runnumber=$runnumber"`
   if [ -z "$mdstfile" ] ; then
	#rm -f $i
	echo "missing MuDst for ${bname}.daq"
   else
	echo $mdstfile >> ${lbn}.${prod}.mudst.list
   fi
done

sed "s/MuDst/tags/g" ${lbn}.${prod}.mudst.list > ${lbn}.${prod}.tags.list

