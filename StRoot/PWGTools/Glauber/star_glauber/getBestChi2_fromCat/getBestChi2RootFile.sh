#!/bin/bash

############################################################
# original author Zaochen Ye
# new STAR users are responsible to improve the script
############################################################

#if nevents=1million
nppValue=$(tail -1 minChi2_rootfile_fullname.dat | cut -c 42-46)
kValue=$(tail -1 minChi2_rootfile_fullname.dat | cut -c 55-59)
xValue=$(tail -1 minChi2_rootfile_fullname.dat | cut -c 68-72)
effValue=$(tail -1 minChi2_rootfile_fullname.dat | cut -c 83-87)
#if nevents=100k
#nppValue=$(tail -1 minChi2_rootfile_fullname.dat | cut -c 41-45)
#kValue=$(tail -1 minChi2_rootfile_fullname.dat | cut -c 54-58)
#xValue=$(tail -1 minChi2_rootfile_fullname.dat | cut -c 67-71)
#effValue=$(tail -1 minChi2_rootfile_fullname.dat | cut -c 82-86)

#nppValue=$(tail -1 minChi2_file.list | cut -c 42-46)
#kValue=$(tail -1 minChi2_file.list | cut -c 55-59)
#xValue=$(tail -1 minChi2_file.list | cut -c 128-132)
#effValue=$(tail -1 minChi2_file.list | cut -c 143-147)

echo "The parameters to best match Glauber Simulation to Corrected Refmult in real data are found as:"
echo "nppValue: " $nppValue
echo "kValue: "   $kValue
echo "xValue: "   $xValue
echo "effValue:"  $effValue

inDir=../RatioChi2Files
outDir=bestChi2RootFile

#HERE                     v
cp ${inDir}/chi2_nevents1000000_npp${nppValue}-${nppValue}_k${kValue}-${kValue}_x${xValue}_${xValue}_eff${effValue}.root ${outDir}
cp ${inDir}/Ratio_npp${nppValue}_k${kValue}_x${xValue}_eff${effValue}.root ${outDir}
echo "the root files are copied to dir of ${outDir}"
exit
