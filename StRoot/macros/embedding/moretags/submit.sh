#!/bin/sh

if [ ! -d sums ] ; then
   mkdir sums
fi
if [ ! -d logs ] ; then
   mkdir logs
fi
if [ ! -d output ] ; then
   mkdir output 
fi
star-submit-template -template ./Scheduler.xml -entities MuDstList=`pwd`/$1,STARLib=$2
