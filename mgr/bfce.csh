#! /usr/local/bin/tcsh -f
source /star/u2e/starreco/.tcshrc 
source /afs/rhic/rhstar/group/.stardev
echo "Start $0 with $argv on" `date`
perl $STAR/mgr/bfc_emd $argv
