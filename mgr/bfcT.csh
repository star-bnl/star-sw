#! /usr/local/bin/tcsh -f
source /star/u/starreco/.tcshrc 
source /afs/rhic/rhstar/group/.stardev
unsetenv NODEBUG
stardev
echo "Start $0 with $argv on" `date`
perl $STAR/mgr/bfcT $argv

