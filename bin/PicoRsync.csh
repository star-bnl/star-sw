#! /usr/bin/tcsh -f
set day = `basename $PWD`; echo "day = ${day}"
rsync -avz -h                        \
    --include='*.root'                  \
    --exclude='*.log' --exclude='*.dat' \
    ./ rftpexp01.rhic.bnl.gov:/gpfs01/star/pwg/fisyak/Pico/14GeV_2019_TFG19e/${day}
