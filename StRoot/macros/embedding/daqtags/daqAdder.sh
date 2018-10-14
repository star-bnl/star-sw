#!/bin/bash

#Concatenates daq files of the same run number
daqDir=$1
outDir=$2

filetype="st_physics_adc"
maxdaqsize=2000   #in MB, if the total combined daq file size is larger, no combination

uniqueRunNumber=$(ls $daqDir | cut -d"_" -f4 | sort | uniq)
nRunNumbers=$(ls $daqDir | cut -d"_" -f4 | sort | uniq | wc -l)

echo "Number of Unique Runs: " $nRunNumbers

echo > combined_runs.list

for i in `seq 1 $nRunNumbers`;
do 

    runNumber=$(echo $uniqueRunNumber | awk -vv=$i '{ print $v }')
    nfile=$(ls $daqDir/*$runNumber* | wc -l)
    nsize=$(du -cm $daqDir/*$runNumber* | grep total | awk '{print $1}')

    if [[ $nfile -gt 1 && $nsize -lt $maxdaqsize ]]; then
	 newFileName=$outDir/"${filetype}_"$runNumber"_raw_0000000.daq"

	 echo $runNumber $nfile $nsize $newFileName
	 
	 echo $runNumber >> combined_runs.list

	 cat $daqDir/*$runNumber*.daq > $newFileName
    else
	 for j in $(ls $daqDir/*$runNumber*.daq)
	 do
	    bn=$(basename $j)
	    echo $runNumber $nfile $nsize $outDir/$bn
	    cp -p $daqDir/$bn $outDir/
	 done
    fi

done
