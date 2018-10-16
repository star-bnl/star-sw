#!/bin/sh
#usage: 
#./findfailedslr.sh starTask_2017xxxx_1xx_1xx.list
#for failed tasks for a cori job, generate a new pair of .list and .slr for resubmission.
#the number of nodes will be scaled down as well.

if [[ ! $HOST =~ "cori" ]] ; then
   echo this code can only be used for Cori!
   exit
fi

if [ ! $# -eq 1 ] ; then
   echo ""
   echo " Usage : $0 [starTask_2017xxxx_1xx_1xx.list] "
   echo ""
   echo ""
   exit
fi

echo I am running in $PWD
echo parsing the request information from the configuration file \"preparexmlslr.sh\"

trg=`grep "\-trg" $PWD/preparexmlslr.sh | awk -F"-trg |-production" '{print $2}'`
trgset=`echo $trg`
part=`grep "\-particle" $PWD/preparexmlslr.sh | awk '{print $2}'`
particle=`echo $part`
reqid=`grep "\-r" $PWD/preparexmlslr.sh | awk '{print $2}'`
daqdir=`grep "\-daq\ " $PWD/preparexmlslr.sh | awk '{print $2}'`
ndaq=`find $daqdir/*.daq | wc -l`

echo trgset: $trgset
echo particle name: $particle
echo request ID: $reqid
echo number of daq files: $ndaq

tlist=$1
tlbn=`basename $tlist .list`
slr=`grep -H $tlist *.slr | awk -F':' '{print $1}'`
slrbn=`basename $slr .slr`
echo the input task list file: $tlist
if [ -z "$slr" ] ; then
   echo its slr file does not exist, quit now
   exit
fi
echo found its slr file: $slr

slurmid=`grep -H $tlist mon.farmer-* | awk -F':' '{print $1}' | awk -F'-' '{print $2}'`
if [ -z "$slurmid" ] ; then
   echo CAUTION: its slurm id does not exist, job has not started to run?! Please double check!
   exit
fi
echo found its slurm job id: $slurmid

echo its scratch working dir: $CSCRATCH/starFarm-$slurmid/
echo listing ...
ls -l $CSCRATCH/starFarm-$slurmid/

newtlist=${tlbn}_resub.list
newslr=${slrbn}_resub.slr

if [[ -f $newtlist || -f $newslr ]] ; then
   rm -f $newtlist 
   rm -f $newslr
fi

datadir="$CSCRATCH/embedding/$trgset"
echo ""
echo "now checking the data in $datadir/"

begin=`echo $tlbn | awk -F'_' '{print $3}'`
end=`echo $tlbn | awk -F'_' '{print $4}'`
for (( ifset=$begin ; $ifset - $(($end+1)) ; ifset++ ))
do
   fsetp=$datadir/${particle}_${ifset}_${reqid}
   if [ "$ifset" -eq $begin ] ; then
	srcdirs="$fsetp"
   else
	srcdirs="$srcdirs $fsetp"
   fi
done

find $srcdirs -name "*.log.gz" > tmplog$$.list
find $srcdirs -name "*.minimc.root" > tmpminimc$$.list

nfailed=0
while read line
do
   fn=`echo $line | awk '{print $4}'`
   fset=`echo $line | awk '{print $5}'`
   logfile=`grep "${fn}.r4s_${fset}.log.gz" tmplog$$.list`
   if [ -z "$logfile" ] ; then
	echo "found one possible failed task with no log file"
	echo $line >> $newtlist
	nfailed=$(($nfailed+1))
	continue
   fi

   minicheck=`grep "${fn}.minimc.root" tmpminimc$$.list | grep "${particle}_${fset}_${reqid}"`
   aborted=`zgrep "StChain::Embedding" $logfile`
   buserr=`zgrep "Bus error" $logfile`
   if [[ -z "$minicheck" || -z "$aborted" || ! -z "$buserr" ]] ; then
	eofcheck=`zgrep "StIOMaker::Make() == StEOF" $logfile`
	zeroevent=`zgrep "StAnalysisMaker::Finish() Processed 0 events" $logfile`
	if [[ -z "$eofcheck" || -z "$zeroevent" ]] ; then
	   echo "found one possible failed task, its log file is:"
	   echo $logfile
	   echo $line >> $newtlist
	   nfailed=$(($nfailed+1))
	fi
   fi
done < $tlist 

if [ $nfailed -eq 0 ] ; then
   echo ""
   echo this job finished properly!
   echo ""
fi

if [ $nfailed -gt 0 ] ; then
   cp $slr $newslr
   sed -i "s/$tlist/$newtlist/g" $newslr
   nnode=`grep "#SBATCH -N" $slr | awk '{print $3}'`
   ntask=`cat $tlist |wc -l`
   nnewtask=`cat $newtlist |wc -l`
   nnewnode=`echo "$nnewtask/$ntask*($nnode-1)+2" |bc -l |awk -F'.' '{print $1}'`
   sed -i "s/SBATCH -N $nnode/SBATCH -N $nnewnode/g" $newslr

   nskew=`grep "export SKEW=" $slr | awk '{print $2}' | awk -F'=' '{print $2}'`
   nnewskew=`echo "($nnewnode-1)/($nnode-1)*$nskew" |bc -l |awk -F'.' '{print $1}'`
   sed -i "s/export SKEW=$nskew/export SKEW=$nnewskew/g" $newslr

   if [ $nnewnode -lt 4 ] ; then
	sed -i "s/#SBATCH  --partition regular -t 35:30:00   -J starFarm-reg/#-SBATCH  --partition regular -t 35:30:00   -J starFarm-reg/g" $newslr
	sed -i "s/#-SBATCH  --partition regular -t 35:30:00 --qos=premium   -J starFarm-prem/#SBATCH  --partition regular -t 35:30:00 --qos=premium   -J starFarm-prem/g" $newslr
   fi

   echo ""
   echo "CAUTION: found $nfailed failed tasks!"
   echo "$newslr and $newtlist are prepared"
   echo "please submit with"
   echo "sbatch $newslr"
   echo ""

fi

rm -f tmplog$$.list tmpminimc$$.list

