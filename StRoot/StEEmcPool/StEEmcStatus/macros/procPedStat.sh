#!/bin/bash

nr=0

mkdir -p StatFiles

cat runList |
while read RUN; 
do echo "${RUN}";

root4star -b -q -l pedStat.C\(\"${RUN}\",$nr\)

nr=$[$nr+1]

done

cp StRoot/StEEmcPool/StEEmcStatus/macros/procErrs.sh StatFiles/
cp StRoot/StEEmcPool/StEEmcStatus/macros/DistrStat2Sectors.C StatFiles/

exit

