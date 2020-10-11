#! /usr/bin/tcsh -f
echo "pwd = $PWD"
set dir = `basename ${PWD}`; #11p5GeV.B
set topdir = `dirname ${PWD}`;
set top = `basename  ${topdir}`
set log =  rsynchDAQ.`date +%m%d%y%H`.log;
rsync -avrz -h                        \
    --include='*adc*.daq'                  \
    --exclude='*.*' \
     rftpexp01.rhic.bnl.gov:/gpfs01/star/daq/2019 . >>& ${log}
