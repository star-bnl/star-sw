#! /bin/tcsh -f
cd /net/l404/data/fisyak/Tpc/Current/2020 
root.exe -b -q lmysql.C 'MakeTpcAvgPowerSupply.C+(2020)' > & MakeTpcAvgPowerSupply.`date +%m%d%y:%H%M`.log
if ($?) exit 1;
#rsync -avz -h                        \
#    --include='*.root'                  \
#    --exclude='*.log' --exclude='*.dat' \
#    ./ rftpexp01.rhic.bnl.gov:/gpfs01/star/subsys-tpc/fisyak/Tpc/Current/2020/ >& rsync.`date +%m%d%y`.log
#foreach f (`ls -1d *.root`)
#  mv ${f} ${f}.HOLD
#end
ls -1d TpcAvgPowerSupply.*.root
if ($?) exit 0;
put2DB.pl 'StarDb/Calibrations/tpc/TpcAvgPowerSupply.*.root'
