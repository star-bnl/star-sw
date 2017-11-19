#!/bin/sh
#show statistics info of each Fset, applicable at both PDSF and Cori

if [[ $# -eq 0 || $# -gt 2 ]] ; then
   echo ""
   echo " Usage : $0 [Fset#] "
   echo "      or $0 [Fset_begin#] [Fset_end#] "
   echo ""
   echo ""
   exit
fi

begin=$1
if [ $# -eq 1 ] ; then
   end=$(($1+1))
else
   if [ $1 -gt $2 ] ; then
	echo "wrong parameters! the first fset# should not be larger than the second! "
	exit
   fi
   end=$(($2+1))
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
daqdir=`grep "\-daq" $PWD/preparexmlslr.sh | awk '{print $2}'`
ndaq=`find $daqdir/*.daq | wc -l`

for (( ifset=$begin ; $ifset - $end ; ifset++ ))
do

fsetp=$outp/$trgset/${particle}_${ifset}_${reqid}

echo ""
if [ ! -d "$fsetp" ] ; then
   echo CAUTION: Fset# $ifset directory can not be found in $outp/$trgset 
   if [ $ifset -eq $(($end-1)) ] ; then
	echo ""
   fi
   continue
fi
echo "Fset# $ifset is found at $fsetp"
echo "The directory size is "`du -sh $fsetp | awk '{print $1}'`
echo "Now start to scan the log files..."


total=0
count=0
allevts=0
storedevts=0

for i in `find $fsetp/ -name "*.log.gz"`
do
#echo $i
if [ -f tmplog.txt ] ; then
   rm -f tmplog.txt
fi
gzip -cd $i | tail -n 1000 > tmplog.txt
jtime=`grep "StChain::Embedding" tmplog.txt | awk -F '=' '{print $2}' | awk '{print $1}'`
if [ -z $jtime ] ; then
   echo CAUTION: corrupted task found!
   echo $i
   continue
fi
count=$(($count+1))
total=`echo "$total+$jtime" |bc -l`
nevents=`grep "Total events processed" tmplog.txt | awk -F ':' '{print $4}' | awk '{print $1}'`
#echo $nevents events in this daq file
nanaevents=`grep "StAnalysisMaker::Finish" tmplog.txt | awk '{print $5}'`
#echo $nanaevents events accepted for embedding in this daq file
allevts=`echo "$allevts+$nevents" |bc -l`
storedevts=`echo "$storedevts+$nanaevents" |bc -l`
done
if [ -f tmplog.txt ] ; then
   rm -f tmplog.txt
fi

echo "Scanned Fset# $ifset data:" 
echo "total # of daq files:" $ndaq "; total # of log files:" $count "; total # of daq events:" $allevts
echo "total # of embedded events:" $storedevts "; total CPU*Hours:" `echo "$total/3600+0.5" | bc -l | cut -d. -f1` "; average CPU*Hours per daq file:" `echo "$total/$count/3600+0.5" | bc -l | cut -d. -f1`
if [ $count -lt $ndaq ] ; then
   echo CAUTION: $(($ndaq-$count))/$ndaq daq files have not been processed properly!
   if [[ $HOST =~ "cori" ]] ; then
	echo Please use \'findfailedslr.sh\' to do some further check.
   else
	echo Please use \'findfailedxml.sh\' to do some further check, if the data was produced at PDSF.
	echo or use \'findfailedslr.sh\' at Cori node if the data was first produced at Cori.
   fi
fi

if [ $ifset -eq $(($end-1)) ] ; then
   echo ""
fi

done
