#!/bin/sh

total=0
count=0
allevts=0
storedevts=0

for i in `find $1 -name "*.log.gz"`
do
echo $i
jtime=`zcat $i| tail -n 1000 | grep "StChain::Embedding" | awk -F '=' '{print $2}' | awk '{print $1}'`
if [ -z $jtime ] ; then
   continue
fi
count=$(($count+1))
total=`echo "$total+$jtime" |bc -l`
nevents=`zcat $i| tail -n 1000 | grep "Total events processed" | awk -F ':' '{print $4}' | awk '{print $1}'`
#echo $nevents
nanaevents=`zcat $i| tail -n 1000 | grep "StAnalysisMaker::Finish" | awk '{print $5}'`
#echo $nanaevents
allevts=`echo "$allevts+$nevents" |bc -l`
storedevts=`echo "$storedevts+$nanaevents" |bc -l`
done
echo "total # of files:" $count "; total # of daq events:" $allevts "; total # of embedded events:" $storedevts "; total CPU*Hours:" `echo "$total/3600+0.5" | bc -l | cut -d. -f1` "; average CPU*Hours per file:" `echo "$total/$count/3600+0.5" | bc -l | cut -d. -f1`
