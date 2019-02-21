#! /bin/tcsh -f
cd /net/l404/data/fisyak/Tpc/Lana/2019 
LoopOverLaserTrees.pl
if ($?) exit 0;
/net/l402/data/fisyak/STAR/packages/.DEV2/scripts/star-submit ~/xml/LoopOverLaserTrees.xml
foreach f (`ls -1d *.root`)
  mv ${f} ${f}.HOLD
end

