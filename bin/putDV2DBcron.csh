#! /bin/tcsh -f
#cd /net/l404/data/fisyak/Tpc/Lana/2021/put2DB
cd /net/l404/data/fisyak/Tpc/Lana/2022/put2DB
ls -1d StarDb/Calibrations/tpc/tpc*.C
if ($?) exit 0;
#put2DB.pl StarDb/Calibrations/tpc/tpcDriftVelocity.2021*.C
put2DB.pl StarDb/Calibrations/tpc/tpcDriftVelocity.202*.C


