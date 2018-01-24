#!/bin/sh
#The robot (daemon) to automatically submit multiple fsets.
#why need a daemon, since there is a limit in PDSF SLURM queue, each user can only submit 5000 jobs at most.
#Usage:
#1. modify fset (the first fset ID) and nset (the total # of fsets)
#2. run with:
#   nohup ./subrobot.sh >& subxxx_xxx.log &
#3. you can logout now. the daemon will run in bg. check the status by looking at the log file. 
#   if you want to kill the daemon, please memorize the pdsf node #, where the command was issued.
#   logon the node, and 'ps aux|grep robot' to retrive the job ID, 'kill $jobID'

fset="101"
nset="52"

if [[ $HOST =~ "cori" ]] ; then
   echo "this code can only be used for PDSF!"
   exit
fi

daqdir=`grep "\-daq" preparexmlslr.sh | awk '{print $2}'`
nfsetjob=`ls $daqdir/*daq |wc -l`
echo $nfsetjob jobs per FSET!

eset=$(($fset+$nset))
for (( i=$fset ; $i - $eset ; i++ ))
do

   #a small robot
   echo "now working on FSET# $i ..."
   nqueue=`squeue --array | grep staremb | wc -l`
   nall=$(($nqueue+$nfsetjob))

   echo "now $nqueue staremb jobs in the queue..."
   while [ $nall -gt 4500 ] 
   do
	echo "no enough slots, wait for ten minutes..."
	sleep 10m
	nqueue=`squeue --array | grep staremb | wc -l`
	nall=$(($nqueue+$nfsetjob))
   done
   echo "./submitxml.csh $i"
   ./submitxml.csh $i
done
