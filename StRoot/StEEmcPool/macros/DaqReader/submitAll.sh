#!/bin/sh

inpPath=/star/data03/daq/2012
outPath=./

cat runList |
while read run 
do 

day="${run:2:3}"
echo ${run}
echo $day

#exit

mkdir -p $outPath/$run/

#exit

for file in $( ls /$inpPath/$day/$run ); do
  ./emchist $inpPath/$day/$run/$file
  mv *.root $outPath/$run
  echo $file
done

#exit

done

