#! /usr/bin/tcsh -f
#set day = `basename $PWD`; echo "day = ${day}"
rsync -avz -h                        \
    --include='*MuDst.root'                  \
    --exclude='*.*'  \
    ./ rftpexp01.rhic.bnl.gov:/gpfs01/star/scratch/fisyak/reco/2021/RF/DEV2/7p7GeV_2021.C
#    ./ rftpexp01.rhic.bnl.gov:/gpfs01/star/subsys-tpc/fisyak/Pico/2020/TFG21b/RF 
