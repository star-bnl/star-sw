#!/bin/sh
#loop over all daq files in the daqfilelist, run daqFileChopper with its corresponding .chopper.txt file

if [ $# -ne 2 ] ; then
   echo ""
   echo " Usage : $0 [DaqList] [OutDir] "
   echo ""
   exit
fi

echo ""
echo the input daq file list: $1

outDir=$2
echo the output daq directory: $outDir

inDir=output

if [ ! -d $outDir ]; then
   mkdir -p $outDir
fi
if [ ! -d logs ]; then
   mkdir logs
fi

for i in `cat $1`
do
   ls -l $i
   bn=`basename $i .daq`
   goodevts=`find $inDir/ -name "$bn".chopper.txt`
   echo $goodevts

   eventList=$(cat $goodevts | cut -f2 | tr '\n' ' ')
   echo $eventList
   daqFileChopper $i "-eventnum" $eventList 2>logs/${bn}.daqchop.log 1>${outDir}/${bn}.daq

done

