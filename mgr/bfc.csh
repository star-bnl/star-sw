#! /usr/local/bin/tcsh -f
source /star/u2e/starreco/.tcshrc 
source /afs/rhic/rhstar/group/.starnew
echo "Start $0 with $argv on" `date`
perl $STAR/mgr/bfc $argv
