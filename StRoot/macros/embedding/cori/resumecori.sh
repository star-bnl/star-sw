#!/bin/sh
#usage: 
#./resumecori.sh starTask_2017xxxx_1xx_1xx.list
#for a broken cori job, generate a new pair of .list and .slr for resubmission of those not finished tasks only.
#the number of nodes will be scaled down.

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

tlist=$1
tlbn=`basename $tlist .list`
slr=`grep -H $tlist *.slr | awk -F':' '{print $1}'`
slrbn=`basename $slr .slr`
echo the input task list file: $tlist
if [ -z "$slr" ] ; then
   echo its slr file does not exist, quit now
   exit
fi
echo its slr file: $slr

newtlist=${tlbn}_rest.list
newslr=${slrbn}_rest.slr

rm -f $newtlist $newslr

slurmid=`grep -H $tlist mon.farmer-* | awk -F':' '{print $1}' | awk -F'-' '{print $2}'`
if [ -z "$slurmid" ] ; then
   echo its slurm id does not exist, quit now
   exit
fi
echo its slurm job id: $slurmid

find $CSCRATCH/starFarm-$slurmid/ -name  "st*.r4s_*.log" > tmp.list
echo its scratch working dir: $CSCRATCH/starFarm-$slurmid/
ls -l $CSCRATCH/starFarm-$slurmid/
bswitch=0

cat $tlist | while read line
do
   fn=`echo $line | awk '{print $4}'`
   fset=`echo $line | awk '{print $5}'`
   ftest=`grep ${fn}.r4s_${fset}.log tmp.list`
   #echo $fn $fset $ftest
   
   if [ ! -z $ftest ] ; then
	bswitch=1
   fi
   if [ $bswitch -eq 1 ] ; then
	echo $line >> $newtlist
   fi
done

if [ $bswitch -eq 0 ] ; then
   echo this job finished properly!
fi

if [ $bswitch -eq 1 ] ; then
   cp $slr $newslr
   sed -i "s/$tlist/$newtlist/g" $newslr
   nnode=`grep "#SBATCH -N" $slr | awk '{print $3}'`
   ntask=`cat $tlist |wc -l`
   nnewtask=`cat $newtlist |wc -l`
   nnnode=`echo "scale=7;$nnewtask/$ntask*($nnode-1.0)" |bc -l`
   nnewnode=`echo "scale=0;($nnnode+2)/1.0" |bc -l`
   sed -i "s/SBATCH -N $nnode/SBATCH -N $nnewnode/g" $newslr
fi

rm -f tmp.list
