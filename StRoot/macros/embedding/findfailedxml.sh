#!/bin/sh

#usage: in a pdsf embedding job submission directory, where the .session.xml file is, run
#./findfailedxml.sh
#
#the code loops over # of daq files, find whether there is log.gz file, if not, printout the #, if yes,
#keep looking for the minimc.root file, if not, return #.

if [[ ! $HOST =~ "pdsf" && ! $HOST =~ "rcas" ]] ; then
   echo this code can only be used for PDSF and RCF!
   exit
fi

echo I am running in $PWD
echo parsing the request information from the configuration file \"preparexmlslr.sh\"

part=`grep "\-particle" $PWD/preparexmlslr.sh | awk '{print $2}'`
particle=`echo $part`
reqid=`grep "\-r" $PWD/preparexmlslr.sh | awk '{print $2}'`
daqdir=`grep "\-daq\ " $PWD/preparexmlslr.sh | awk '{print $2}'`
ndaq=`find $daqdir/*.daq | wc -l`

echo particle name: $particle
echo request ID: $reqid
echo number of daq files: $ndaq

subfile=resubfailedxml.sh

if [ -f "$subfile" ] ; then
   rm -f $subfile
fi
allgood=""

#echo "#!/bin/sh" > $subfile
echo "cd "$PWD > $subfile

for i in `find $PWD/*.session.xml`
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

   if [[ $HOST =~ "rcas" ]] ; then
	emlog=`grep "setenv EMLOGS" $i | awk '{print $3}'`
	xmllog=${datadir}${particle}_${reqid}/LOG
   else
	emlog=${datadir}${particle}_${fset}_${reqid}
	xmllog=$emlog
   fi
   echo embedding log directory: $emlog
   echo xml log directory: $xmllog

   if [[ $HOST =~ "pdsf" ]] ; then
	cshdirtmp=`grep 'sched$JOBID.csh $EMLIST/' $i` 
	cshdir=`echo $cshdirtmp | awk '{print $3}' |xargs dirname`
	echo csh script directory: $cshdir
   fi

   nlogs=`find ${emlog}/ -name "*.log*" |wc -l`
   echo Number of non-zero size log files: $nlogs

   #if [ $nlogs -lt 890 ] ; then
   echo "checking FSET "$fset"..."
   find ${xmllog}/ -name "*.log*" > tmplog$$.list
   find ${datadir}${particle}_${fset}_${reqid}/ -name "*.minimc.root" > tmpminimc$$.list
   #fi

   nfailed=0
   for (( ijob=0 ; $ijob - $ndaq ; ijob++ ))
   do
	if [[ $HOST =~ "rcas" ]] ; then
	   xmllogfile=`grep "${bn}_${ijob}\." tmplog$$.list`
	   stfile=`basename $xmllogfile .log | awk -F"_${bn}_" '{print $1}'`
	   logfile=`find ${emlog}/ -name "${stfile}*.log"`
	else
	   logfile=`grep "${bn}_${ijob}\." tmplog$$.list`
	fi
	#echo $logfile
	if [ -z "$logfile" ] ; then
	   echo "found one possible failed task, ${bn}_${ijob}, no log file!"
	   if [ $nfailed -ne "0" ] ; then
		echo -n "," >> $subfile
	   else
		if [[ $HOST =~ "rcas" ]] ; then
		   echo -n "star-submit -r " >> $subfile
		else
		   echo -n "sbatch --array=" >> $subfile
		fi
	   fi
	   echo -n $ijob >> $subfile
	   nfailed=$(($nfailed+1))
	else
	   if [[ $HOST =~ "rcas" ]] ; then
		minifile=`echo $stfile | awk -F. '{print $1}'`
		cgrep="grep"
	   else
		bnjob=`basename $logfile .log.gz`
		#echo $bnjob
		minifile=`echo $bnjob | awk -F. '{print $1}'`
		cgrep="zgrep"
	   fi
	   #echo $minifile
	   minicheck=`grep $minifile tmpminimc$$.list`
	   aborted=`$cgrep "StChain::Embedding" $logfile`
	   buserr=`$cgrep "Bus error" $logfile`
	   if [[ -z "$minicheck" || -z "$aborted" || ! -z "$buserr" ]] ; then
		eofcheck=`$cgrep "StIOMaker::Make() == StEOF" $logfile`
		zeroevent=`$cgrep "StAnalysisMaker::Finish() Processed 0 events" $logfile`
		if [[ -z "$eofcheck" || -z "$zeroevent" ]] ; then
		   echo "found one possible failed task, ${bn}_${ijob}, its log file is:"
		   echo $logfile

		   if [[ $HOST =~ "pdsf" ]] ; then
			cshfile=`find ${datadir}${particle}_${reqid}/ -name "sched${bn}_${ijob}.csh"`
			if [ ! -z "$cshfile" ] ; then
			   echo "its csh file has been moved to $cshfile"
			   echo "move it back to $cshdir"
			   mv $cshfile $cshdir
			fi
		   fi

		   if [ $nfailed -ne "0" ] ; then
			echo -n "," >> $subfile
		   else
			if [[ $HOST =~ "rcas" ]] ; then
			   echo -n "star-submit -r " >> $subfile
			else
			   echo -n "sbatch --array=" >> $subfile
			fi
		   fi
		   echo -n $ijob >> $subfile
		   nfailed=$(($nfailed+1))
	      fi
	   fi
	fi
   done

   if [ $nfailed -gt 0 ] ; then
	if [[ $HOST =~ "rcas" ]] ; then
	   echo " ${bn}.session.xml" >> $subfile
	else
	   echo " array_0_$(($ndaq-1))_"`basename $bn`".slr" >> $subfile
	fi
   fi
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
rm -f tmplog$$.list tmpminimc$$.list

