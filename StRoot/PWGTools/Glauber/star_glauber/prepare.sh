#!/bin/bash

outDir=../glauberOut
mkdir -p ${outDir}
ln -s ${outDir} .

mkdir -p ${outDir}/temfor_rm
mv ${outDir}/LOG    ${outDir}/temfor_rm/
mv ${outDir}/LIST   ${outDir}/temfor_rm/
mv ${outDir}/table  ${outDir}/temfor_rm/
mv ${outDir}/figure ${outDir}/temfor_rm/

mkdir -p ${outDir}/LOG
mkdir -p ${outDir}/LIST
mkdir -p ${outDir}/table
mkdir -p ${outDir}/figure
mkdir -p ${outDir}/output
mkdir -p ${outDir}/RatioChi2Files
mkdir -p ${outDir}/LOG_Scan

ln -s ${outDir}/LOG
ln -s ${outDir}/LIST
ln -s ${outDir}/table
ln -s ${outDir}/figure
ln -s ${outDir}/output
ln -s ${outDir}/RatioChi2Files
ln -s ${outDir}/LOG_Scan


echo "after mv the old output, LOG, LIST, table, figure files to the temfor_rm, let's delele temfor_rm"
rm -rf ${outDir}/temfor_rm &

