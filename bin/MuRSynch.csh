#! /usr/bin/tcsh -f
#set day = `basename $PWD`; echo "day = ${day}"
rsync -avz -h                        \
    --include='*MuDst.root'                  \
    --exclude='*.*'  \
    ./ rftpexp01.rhic.bnl.gov:/gpfs01/star/subsys-tpc/fisyak/Pico/2020/TFG21b/RF 
