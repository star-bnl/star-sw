#!/bin/csh

source /star/u/starreco/.tcshrc 
setenv NODEBUG yes
starver dev

echo "Start $0 with $argv on `date`"
perl $STAR/mgr/bfcc $argv


