#! /usr/bin/tcsh -f
rsync -avz -h                        \
    --include='*.root'                  \
    --exclude='*.log' --exclude='*.dat' \
    ./ rftpexp01.rhic.bnl.gov:/gpfs01/star/subsys-tpc/fisyak/Tpc/Current/2019/

