#! /usr/local/bin/tcsh -f
source /star/u/starreco/.tcshrc 
source /afs/rhic/rhstar/group/.starnew
setenv NODENUG yes
starver 01e
echo "Start $0 with $argv on" `date`
perl $STAR/mgr/bfcc $argv

