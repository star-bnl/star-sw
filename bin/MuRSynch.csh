#! /usr/bin/tcsh -f
#set day = `basename $PWD`; echo "day = ${day}"
 rsync -avzr -h --include='*MuDst.root' ./ rftpexp02.rhic.bnl.gov:/gpfs01/star/scratch/fisyak/reco/2021/RF/TFG21c.B/7p7GeV_2021
#rsync -avzr -h --include='*MuDst.root' ./ rftpexp01.rhic.bnl.gov:/gpfs01/star/scratch/fisyak/reco/2021/RF/TFG21c.B/7p7GeV_2021
#    --exclude='*.*'  \
#    ./ rftpexp01.rhic.bnl.gov:/gpfs01/star/scratch/fisyak/reco/2021/RF/DEV2/7p7GeV_2021.C
#    ./ rftpexp01.rhic.bnl.gov:/gpfs01/star/subsys-tpc/fisyak/Pico/2020/TFG21b/RF 
