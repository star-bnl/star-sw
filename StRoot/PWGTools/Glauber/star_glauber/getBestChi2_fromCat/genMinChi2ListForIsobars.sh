#!/bin/bash
#before run this script, be sure that, the number of event should be on the same order: 1000000, otherwise, it won't work.

rm chi2_valuesCOMBINED
rm chi2_valuesCombinedAndOrdered
rm chi2_valuesRU
rm chi2_valuesZR

#First isolate the fit parameters and sort them so you can compare the same parameters for each isobar
echo "generate chi2_values"
cat catLOG_for_MinChi2_ZR | cut -d "=" -f2 | sort -g > chi2_valuesZR
cat catLOG_for_MinChi2_RU | cut -d "=" -f2 | sort -g > chi2_valuesRU

#numLines=$(wc -l chi2_valuesZR | cut -d " " -f1)

#     HERE    v    Edit the number below to correspond to the number of lines in chi2_valuesZR
for i in {1..256}
do
  chi2_zr=$(sed "${i}q;d" chi2_valuesZR | cut -d "," -f5 | cut -d "/" -f1)
  dof_zr=$(sed "${i}q;d" chi2_valuesZR | cut -d "," -f5 | cut -d "/" -f2)
  chi2_ru=$(sed "${i}q;d" chi2_valuesRU | cut -d "," -f5 | cut -d "/" -f1)
  dof_ru=$(sed "${i}q;d" chi2_valuesRU | cut -d "," -f5 | cut -d "/" -f2)
  chi2_combined=$(awk '{print ($1+$3)/($2+$4)}' <<<"${chi2_zr} ${dof_zr} ${chi2_ru} ${dof_ru}") 
  echo $chi2_combined >> chi2_valuesCOMBINED
done

cat chi2_valuesCOMBINED | sort -g > chi2_valuesCombinedAndOrdered 

echo "generated chi2_values"

minChi2=$(head -n 1 chi2_valuesCombinedAndOrdered)

echo "found minimum combined chi2: $minChi2"
chiLineNumber=$(grep -n "${minChi2}" chi2_valuesCOMBINED | cut -d ":" -f1)
echo "Best chi2 found at line number: $chiLineNumber of chi2_valuesZR and chi2_valuesRU"

nppValue_zr=$(sed "${chiLineNumber}q;d" chi2_valuesZR | cut -c 3-7)
kValue_zr=$(sed "${chiLineNumber}q;d" chi2_valuesZR | cut -c 10-14)
xValue_zr=$(sed "${chiLineNumber}q;d" chi2_valuesZR | cut -c 17-21)
dValue_zr=$(sed "${chiLineNumber}q;d" chi2_valuesZR | cut -c 24-28)
chiValue_zr=$(sed "${chiLineNumber}q;d" chi2_valuesZR | cut -d "," -f5 | cut -d "/" -f1)
dofValue_zr=$(sed "${chiLineNumber}q;d" chi2_valuesZR | cut -d "," -f5 | cut -d "/" -f2)
nppValue_ru=$(sed "${chiLineNumber}q;d" chi2_valuesRU | cut -c 3-7)
kValue_ru=$(sed "${chiLineNumber}q;d" chi2_valuesRU | cut -c 10-14)
xValue_ru=$(sed "${chiLineNumber}q;d" chi2_valuesRU | cut -c 17-21)
dValue_ru=$(sed "${chiLineNumber}q;d" chi2_valuesRU | cut -c 24-28)
chiValue_ru=$(sed "${chiLineNumber}q;d" chi2_valuesRU | cut -d "," -f5 | cut -d "/" -f1)
dofValue_ru=$(sed "${chiLineNumber}q;d" chi2_valuesRU | cut -d "," -f5 | cut -d "/" -f2)

echo "Zr parameters (npp, k, x, d): $nppValue_zr, $kValue_zr, $xValue_zr, $dValue_zr"
echo "Zr chi2/DOF = $chiValue_zr / $dofValue_zr"
echo "Ru parameters (npp, k, x, d): $nppValue_ru, $kValue_ru, $xValue_ru, $dValue_ru"
echo "Ru chi2/DOF = $chiValue_ru / $dofValue_ru"


if (( $(echo "$nppValue_zr != $nppValue_ru" |bc -l) ))
then
  echo "Best fit values do not match for the isobars! Something went wrong!"
  echo "npp_Zr: ${nppValue_zr}, npp_Ru: ${nppValue_ru}"
  exit
fi

nppValue=$nppValue_zr
kValue=$kValue_zr
xValue=$xValue_zr
dValue=$dValue_zr

echo "nppValue: " $nppValue
echo "kValue: "   $kValue
echo "xValue: "   $xValue
echo "dValue: "   $dValue

inDir=../RatioChi2Files
#HERE Change              v
#ls ${inDir}/chi2_nevents1000000_npp${nppValue}-${nppValue}_k${kValue}-${kValue}_x${xValue}_${xValue}_eff*.root > minChi2_fileCOMBINED.list
inDir_Zr=../RatioChi2Files_Zr
inDir_Ru=../RatioChi2Files_Ru
rm -r bestChi2RootFile/Zr
rm -r bestChi2RootFile/Ru
mkdir bestChi2RootFile/Zr
mkdir bestChi2RootFile/Ru
outDir_Zr=bestChi2RootFile/Zr
outDir_Ru=bestChi2RootFile/Ru

cp ${inDir_Zr}/chi2_nevents2000000_npp${nppValue}-${nppValue}_k${kValue}-${kValue}_x${xValue}_${xValue}_eff${dValue}.root ${outDir_Zr}
cp ${inDir_Zr}/Ratio_npp${nppValue}_k${kValue}_x${xValue}_eff${dValue}.root ${outDir_Zr}
cp ${inDir_Ru}/chi2_nevents2000000_npp${nppValue}-${nppValue}_k${kValue}-${kValue}_x${xValue}_${xValue}_eff${dValue}.root ${outDir_Ru}
cp ${inDir_Ru}/Ratio_npp${nppValue}_k${kValue}_x${xValue}_eff${dValue}.root ${outDir_Ru}

echo "the root files are copied to dir of bestChi2RootFile"


exit

#chi2_nevents1000000_npp2.300-2.300_k2.000-2.000_x0.140_0.140_eff0.170.root
