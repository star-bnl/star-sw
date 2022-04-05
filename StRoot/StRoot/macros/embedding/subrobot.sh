#!/bin/sh
#The robot (daemon) to automatically submit multiple fsets.
#why need a daemon, there should be a limit in starembd condor policy, each user can only submit 2500 jobs at most.
#Usage:
#1. modify fset (the first fset ID) and nset (the total # of fsets)
#2. run with:
#   nohup ./subrobot.sh >& subxxx_xxx.log &
#3. you can logout now. the daemon will run in bg. check the status by looking at the log file. 
#   if you want to kill the daemon, please memorize the rcf node #, where the command was issued.
#   logon the node, and 'ps aux|grep robot' to retrive the job ID, 'kill $jobID'

fset="100"
nset="19"

if [[ $HOST =~ "cori" ]] ; then
   echo "this code can only be used for RCF!"
   exit
fi

daqdir=`grep "\-daq\ " preparexmlslr.sh | awk '{print $2}'`
nfsetjob=`ls $daqdir/*daq |wc -l`
echo $nfsetjob jobs per FSET!

eset=$(($fset+$nset))
for (( i=$fset ; $i - $eset ; i++ ))
do

   #a small robot
   echo "now working on FSET# $i ..."
   nqueue=`condor_q submitter $USER | grep $USER | wc -l`
   nall=$(($nqueue+$nfsetjob))

   echo "now $nqueue $USER jobs in the queue..."
   while [ $nall -gt 2500 ] 
   do
	echo "no enough slots, wait for ten minutes..."
	sleep 10m
	nqueue=`condor_q submitter $USER | grep $USER | wc -l`
	nall=$(($nqueue+$nfsetjob))
   done
   echo "./submitxml.csh $i"
   ./submitxml.csh $i
done
