#! /bin/tcsh -f
cd /net/l404/data/fisyak/Tpc/Lana/2021
ls -1d tpc*.C
if ($?) exit 0;
put2DB.pl StarDb/Calibrations/tpc/tpcDriftVelocity.2021*.C


