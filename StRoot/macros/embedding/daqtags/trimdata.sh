#!/bin/bash

if [[ ! $HOST =~ "rcas" ]] ; then
   echo this code can only be used for RCF!
   exit
fi
if [[ ! -d daq || ! -d reco || ! -d tags ]] ; then
   echo daq, reco, or tags directories do not exist, exit...
   exit
fi

echo scanning daq/ ...
for i in `find daq/*.daq`
do
   bname=`basename $i .daq`
   #echo $bname
   mudst=`find reco/ -name "${bname}*"`
   tags=`find tags/ -name "${bname}*"`
   if [[ -z "$mudst" || -z "$tags" ]] ; then
	echo $i has no mudst or tags, deleting...
	rm -f $i
	echo done.
   fi
done
echo scanning reco/ ...
for i in `find reco/*.MuDst.root`
do
   bname=`basename $i .MuDst.root`
   #echo $bname
   daq=`find daq/ -name "${bname}*"`
   tags=`find tags/ -name "${bname}*"`
   if [[ -z "$daq" || -z "$tags" ]] ; then
	echo $i has no daq or tags, deleting...
	rm -f $i
	echo done.
   fi
done
echo scanning tags/ ...
for i in `find tags/*.tags.root`
do
   bname=`basename $i .tags.root`
   #echo $bname
   daq=`find daq/ -name "${bname}*"`
   mudst=`find reco/ -name "${bname}*"`
   if [[ -z "$mudst" || -z "$daq" ]] ; then
	echo $i has no mudst or daq, deleting...
	rm -f $i
	echo done.
   fi
done
