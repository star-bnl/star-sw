#!/bin/sh
#how to run:
#if the scheduler job doesn't run, scancel the job, then run this on terminal (15 tasks in parallel)
#%find sums/sched9EC71*csh > tasks.csh
#%./runterm.sh tasks.csh


if [ $# -ne 1 ] ; then
   echo ""
   echo " Usage : "
   echo " %find sums/schedXXXXX*.csh > tasks.csh "
   echo " %$0 tasks.csh " 
   echo ""
   exit
fi

echo $1

for i in `cat $1`
do
   ls -l $i
   bn=`basename $i .csh`
   tag=`echo $bn | awk -F'_' '{print $1}'`

   count=`ps aux | grep $tag |wc -l`
   while [ $count -gt 15 ]; do
	sleep 2
	count=`ps aux | grep $tag |wc -l`
   done
   if [ -f logs/${bn}.log ] ; then
	rm -f logs/${bn}.log
   fi
   if [ -f logs/${bn}.err ] ; then
	rm -f logs/${bn}.err
   fi
   ./$i 1> logs/${bn}.log 2> logs/${bn}.err &
done

#ramping down
count=`ps aux | grep $tag |wc -l`
while [ $count -gt 0 ]; do
   sleep 1 
   count=`ps aux | grep $tag |wc -l`
done
