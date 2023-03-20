#!/bin/bash
#before run this script, be sure that, the number of event should be on the same order: 1000000, otherwise, it won't work.

rm chi2_values
rm minChi2_file.list

echo "generate chi2_values"
cat catLOG_for_MinChi2 | cut -d "=" -f3 | cut -d ")" -f1 | sort -g > chi2_values
echo "generated chi2_values"

minChi2=$(head -n 1 chi2_values)

echo "found minimum chi2: $minChi2"

nppValue=$(grep "=${minChi2}" catLOG_for_MinChi2 | cut -c 67-71)
kValue=$(grep "=${minChi2}" catLOG_for_MinChi2 | cut -c 74-78)
xValue=$(grep "=${minChi2}" catLOG_for_MinChi2 | cut -c 81-85)

echo "nppValue: " $nppValue
echo "kValue: "   $kValue
echo "xValue: "   $xValue

inDir=../RatioChi2Files
#HERE Change              v
ls ${inDir}/chi2_nevents1000000_npp${nppValue}-${nppValue}_k${kValue}-${kValue}_x${xValue}_${xValue}_eff*.root > minChi2_file.list

exit

#chi2_nevents1000000_npp2.300-2.300_k2.000-2.000_x0.140_0.140_eff0.170.root
