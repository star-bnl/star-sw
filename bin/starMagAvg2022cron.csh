#! /bin/tcsh -f
cd /net/l404/data/fisyak/Tpc/MagnetAvg/2022
root.exe -b -q lDb.C 'Fill_MagnetAvg.C+(2022)' > & Make_MagentAvg.`date +%m%d%y:%H%M`.log
if ($?) exit 1;
ls -1d starMagAvg.*.C
if ($?) exit 0;
put2DB.pl 'StarDb/RunLog/onl/starMagAvg.*.C'
