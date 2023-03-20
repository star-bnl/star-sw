#!/bin/bash
############################################################
# original author Zaochen Ye
# new STAR users are responsible to improve the script
############################################################

rm allout_ZR.list
rm catLOG_for_MinChi2_ZR
rm allout_RU.list
rm catLOG_for_MinChi2_RU

inDir=../

find $inDir/LOG_Scan_Zr -name "*.out" > allout_ZR.list
find $inDir/LOG_Scan_Ru -name "*.out" > allout_RU.list

echo "generating catLOG_for_MinChi2_ZR and catLOG_for_MinChi2_RU, be patient !!!"

for ifile in $(cat allout_ZR.list)
do
        head -206 ${ifile} | tail -1 >> catLOG_for_MinChi2_ZR  #if with centrality definition
done
for ifile in $(cat allout_RU.list)
do
	head -206 ${ifile} | tail -1 >> catLOG_for_MinChi2_RU  #if with centrality definition
done


echo "generated catLOG_for_MinChi2_ZR"
echo "generated catLOG_for_MinChi2_RU"

exit


