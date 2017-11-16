#!/bin/sh
#show statistics info of one Fset, work at both PDSF and Cori

if [ ! $# -eq 1 ] ; then
   echo ""
   echo " Usage : $0 [Fset #] "
   echo ""
   echo ""
   exit
fi

if [[ $HOST =~ "cori" ]] ; then
   outp="$CSCRATCH/embedding"
else
   outp="/global/projecta/projectdirs/starprod/embedding"
fi

part=`grep "\-particle" $PWD/preparexmlslr.sh | awk -F"-particle |-mode" '{print $2}'`
particle=`echo $part`
trg=`grep "\-trg" $PWD/preparexmlslr.sh | awk -F"-trg |-production" '{print $2}'`
trgset=`echo $trg`
reqid=`grep "\-r" $PWD/preparexmlslr.sh | awk '{print $2}'`

fsetp=$outp/$trgset/${particle}_${1}_${reqid}

if [ ! -d "$fsetp" ] ; then
   echo Fset# $1 directory can not be found in $outp/$trgset 
   exit
fi
echo Fset# $1 directory is found in $outp/$trgset, now start to scan ... 

total=0
count=0
allevts=0
storedevts=0

for i in `find $fsetp/ -name "*.log.gz"`
do
echo $i
jtime=`zcat $i| tail -n 1000 | grep "StChain::Embedding" | awk -F '=' '{print $2}' | awk '{print $1}'`
if [ -z $jtime ] ; then
   continue
fi
count=$(($count+1))
total=`echo "$total+$jtime" |bc -l`
nevents=`zcat $i| tail -n 1000 | grep "Total events processed" | awk -F ':' '{print $4}' | awk '{print $1}'`
echo $nevents events in this daq file
nanaevents=`zcat $i| tail -n 1000 | grep "StAnalysisMaker::Finish" | awk '{print $5}'`
echo $nanaevents events accepted for embedding in this daq file
allevts=`echo "$allevts+$nevents" |bc -l`
storedevts=`echo "$storedevts+$nanaevents" |bc -l`
done
echo "total # of files:" $count "; total # of daq events:" $allevts "; total # of embedded events:" $storedevts "; total CPU*Hours:" `echo "$total/3600+0.5" | bc -l | cut -d. -f1` "; average CPU*Hours per file:" `echo "$total/$count/3600+0.5" | bc -l | cut -d. -f1`
