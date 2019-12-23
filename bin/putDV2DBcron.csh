#! /bin/tcsh -f
cd /net/l404/data/fisyak/Tpc/Lana/2020
ls -1d tpc*.C
if ($?) exit 0;
put2DB.pl StarDb/Calibrations/tpc/tpcDriftVelocity.2019*.C StarDb/Calibrations/tpc/tpcDriftVelocity.2020*.C


