#! /usr/bin/tcsh -f
rsync -avz -h                        \
    --include='*.root'                  \
    --exclude='*.log' --exclude='*.dat' \
    ./ rftpexp01.rhic.bnl.gov:/gpfs01/star/subsys-tpc/fisyak/Tpc/Current/2019/

# rsync -avz -h  --remove-source-files   -include='*event.root' ./ /hlt/cephfs/reco/2019/FF
