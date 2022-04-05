#!/bin/bash

if [[ ! $HOST =~ "rcas" ]] ; then
   echo this code can only be used for RCF!
   exit
fi

if [[ ! $# -eq 2 ]] ; then
   echo ""
   echo " Usage : $0 [good_daqfilelist.500.daq.list] [ProdId]"
   echo " output MuDst filelist : good_daqfilelist.500.[ProdId].mudst.list,"
   echo " output tags filelist : good_daqfilelist.500.[ProdId].tags.list. "
   echo ""
   exit
fi


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

