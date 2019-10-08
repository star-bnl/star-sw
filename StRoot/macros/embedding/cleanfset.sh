#!/bin/sh
#totally clean up the temporary and all data&log files if something wrong happened for this fset.
#used for RACF only!

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

if [[ $HOST =~ "rcas" ]] ; then
   outp="/star/embed/embedding"
else
   echo "the marco is only applicable for RACF!"
   exit
fi

part=`grep -a "\-particle" $PWD/preparexmlslr.sh | awk '{print $2}'`
particle=`echo $part`
trg=`grep -a "\-trg" $PWD/preparexmlslr.sh | awk -F"-trg |-production" '{print $2}'`
trgset=`echo $trg`
reqid=`grep -a "\-r" $PWD/preparexmlslr.sh | awk '{print $2}'`

for (( ifset=$begin ; $ifset - $end ; ifset++ ))
do

fsetp=$outp/$trgset/${particle}_${ifset}_${reqid}
echo ""
if [ ! -d "$fsetp" ] ; then
   echo Fset# $ifset directory can not be found in $outp/$trgset 
else
   echo "Fset# $ifset is found at $fsetp"
   echo "The directory size is "`du -sh $fsetp | awk '{print $1}'`
   echo "Deleting it..."
   rm -rf $fsetp
   echo "Done!"
fi

logp=/star/embed/log/${particle}_${ifset}_${reqid}/log
if [ ! -d "$logp" ] ; then
   echo "Fset# $ifset directory can not be found in /star/embed/log"
else
   echo "Fset# $ifset directory is found in /star/embed/log"
   echo "Deleting it..."
   rm -rf $logp
   echo "Done!"
fi

echo "Now parsing the session.xml file"
xmlfile=`grep -l ${particle}_${ifset}_${reqid} *.session.xml`
if [ -f "$xmlfile" ] ; then
   jobid=`basename $xmlfile .session.xml`
   echo "The jobID is $jobid, cleaning the scheduler tmp files..."
   rm -f $xmlfile sched${jobid}.dataset
   rm -f $outp/$trgset/${particle}_${reqid}/LIST/sched${jobid}*
   rm -f $outp/$trgset/${particle}_${reqid}/LOG/*${jobid}*
else
   echo "The $ifset xml is not found!"
fi

rm -rf Localmakerlibs${ifset}.*

if [ $ifset -eq $(($end-1)) ] ; then
   echo ""
fi

done
