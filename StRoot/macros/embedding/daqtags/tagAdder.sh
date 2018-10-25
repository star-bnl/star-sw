#!/bin/bash

#Hadds all tag files of a given runNumber
tagDir=$1
outDir=$2

filetype="st_physics_adc"
tagtype="tags"
#tagtype="moretags"

#Get a list of all the Unique Run Numbers
uniqueRunNumber=$(ls $tagDir | cut -d"_" -f4 | sort | uniq)
nRunNumbers=$(ls $tagDir | cut -d"_" -f4 | sort | uniq | wc -l)

echo "Number of Unique Runs:" $nRunNumbers

for i in `seq 1 $nRunNumbers`;
do 

    runNumber=$(echo $uniqueRunNumber | awk -vv=$i '{ print $v }')
    echo $runNumber

    combrun=`grep $runNumber combined_runs.list`
    if [ -z "$combrun" ]; then
	 #echo no change
	 cp -p $tagDir/*$runNumber*.${tagtype}.root $outDir/
    else
	 newFileName=$outDir/"${filetype}_"$runNumber"_raw_0000000.${tagtype}.root"
	 echo $newFileName

	 hadd $newFileName $tagDir/*$runNumber*.${tagtype}.root
    fi
done

