#! /bin/tcsh -f
cd /net/l404/data/fisyak/Tpc/Current/2019 
root.exe -b -q lmysql.C 'MakeTpcAvgPowerSupply.C+(2019)' > & MakeTpcAvgPowerSupply.`date +%m%d%y`.log
#rsync -avz -h                        \
#    --include='*.root'                  \
#    --exclude='*.log' --exclude='*.dat' \
#    ./ rftpexp01.rhic.bnl.gov:/gpfs01/star/subsys-tpc/fisyak/Tpc/Current/2019/ >& rsync.`date +%m%d%y`.log


