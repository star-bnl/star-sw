#!/bin/sh
#loop over # of daq files, find whether there is log.gz file, if not, printout the #, if yes,
#keep looking for the minimc.root file, if not, return #.

echo I am running in $PWD
echo parsing the request information from the configuration file \"preparexmlslr.sh\"

part=`grep "\-particle" $PWD/preparexmlslr.sh | awk -F"-particle |-mode" '{print $2}'`
particle=`echo $part`
reqid=`grep "\-r" $PWD/preparexmlslr.sh | awk '{print $2}'`
daqdir=`grep "\-daq" $PWD/preparexmlslr.sh | awk '{print $2}'`
ndaq=`find $daqdir/*.daq | wc -l`

echo particle name: $particle
echo request ID: $reqid
echo number of daq files: $ndaq

subfile=submitfailed.sh

if [ -f "$subfile" ] ; then
   rm -f $subfile
fi
allgood=""

#echo "#!/bin/sh" > $subfile
echo "cd "$PWD > $subfile

for i in `find $PWD -name "*.session.xml"`
do
   echo found a session.xml file:
   ls -l $i
   bn=`basename $i .session.xml`
   emdata=`grep "setenv EMOUTPUT" $i`
   #echo $emdata

   fset=`echo $emdata | awk -F"${particle}_|_$reqid" '{print $2}'`
   echo FSET number: $fset
   #if [ $fset -lt $2 ] ; then
   #continue
   #fi

   datadirtmp=`echo $emdata | awk -F"${particle}_|_$reqid" '{print $1}'`
   datadir=`echo $datadirtmp | awk '{print $3}'`
   echo embedding data directory: $datadir

   nlogs=`find ${datadir}/${particle}_${fset}_${reqid}/ -name "*.log.gz" |wc -l`
   echo Number of non-zero size log files: $nlogs

   #if [ $nlogs -lt 890 ] ; then
   echo "checking FSET "$fset"..."
   echo -n "sbatch --array=" >> $subfile
   find ${datadir}/${particle}_${fset}_${reqid}/ -name "*.log.gz" > tmplog.list
   find ${datadir}/${particle}_${fset}_${reqid}/ -name "*.minimc.root" > tmpminimc.list
   #fi

   nfailed=0
   for (( ijob=0 ; $ijob - $ndaq ; ijob++ ))
   do
	logfile=`grep "${bn}_${ijob}\." tmplog.list`
	#echo $logfile
	if [ -z "$logfile" ] ; then
	   if [ $nfailed -ne "0" ] ; then
		echo -n "," >> $subfile
	   fi
	   echo -n $ijob >> $subfile
	   nfailed=$(($nfailed+1))
	else
	   bnjob=`basename $logfile .log.gz`
	   #echo $bnjob
	   minifile=`echo $bnjob | awk -F. '{print $1}'`
	   #echo $minifile
	   minicheck=`grep $minifile tmpminimc.list`
	   aborted=`zgrep "Bus error" $logfile`
	   if [[ -z "$minicheck" || ! -z "$aborted" ]] ; then
		eofcheck=`zgrep "StIOMaker::Make() == StEOF" $logfile`
		zeroevent=`zgrep "StAnalysisMaker::Finish() Processed 0 events" $logfile`
		if [[ -z "$eofcheck" || -z "$zeroevent" ]] ; then
		   echo "found one possible failed task, its log file is:"
		   echo $logfile
		   if [ $nfailed -ne "0" ] ; then
			echo -n "," >> $subfile
		   fi
		   echo -n $ijob >> $subfile
		   nfailed=$(($nfailed+1))
	      fi
	   fi
	fi
   done

   #if [ $nlogs -lt 890 ] ; then
      echo " array_0_$(($ndaq-1))_"`basename $bn`".slr" >> $subfile
   #fi
   echo found $nfailed failed tasks in FSET# $fset data ... 
   if [ $nfailed -gt 0 ] ; then
	allgood="NO"
   fi
done

if [ ! -z $allgood ] ; then
   echo please run \"source $subfile\"
   chmod a+x $subfile
else
   rm -f $subfile
fi
rm -f tmplog.list tmpminimc.list

