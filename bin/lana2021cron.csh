#! /bin/tcsh -f
cd /net/l404/data/fisyak/Tpc/Lana/2021
Lana2021.pl
if ($?) exit 0;
/net/l402/data/fisyak/STAR/packages/.DEV2/scripts/star-submit ~/xml/lana_y2021.xml
