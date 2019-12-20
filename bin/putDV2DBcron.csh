#! /bin/tcsh -f
cd /net/l404/data/fisyak/Tpc/Lana/2019D
ls -1d tpc*.C
if ($?) exit 0;
#put2DB.pl 'StarDb/Calibrations/tpc/tpcDriftVelocity.2019*.C'
put2DB.pl StarDb/Calibrations/tpc/tpcDriftVelocity.2019*.C


