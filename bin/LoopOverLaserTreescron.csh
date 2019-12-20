#! /bin/tcsh -f
cd /net/l404/data/fisyak/Tpc/Lana/2019D 
LoopOverLaserTrees.pl
if ($?) exit 0;
/net/l402/data/fisyak/STAR/packages/.DEV2/scripts/star-submit ~/xml/LoopOverLaserTrees.xml

