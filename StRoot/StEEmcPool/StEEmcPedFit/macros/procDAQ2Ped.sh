#!/bin/bash

out=out/
finalPath=./

#ls for files to be looped over here
ls='/star/institutions/iucf/stevens4/eemc/ped12/200GeV/emc-check/13071063.hist.root'
for file in $ls
do
  mkdir -p $out 
  name="${file/.hist.root/}"  #replaces .ushist.root with blank
  echo $name
  pos=`expr index "$name" k`
  pos2=$pos+3
  pos3=$pos+1
  day="${name:$pos2:3}"
  run="${name:$pos3:8}"
  echo "day $day run $run"
  mkdir -p $finalPath/day$day
  
  #make 1D histograms from output of daq reader
  root4star -b -q -l plDAQ2Ped.C\(\"${name}\"\) > log1 
  echo "Rnnnped.hist.root file complete"
  mv log1 $out
  
  #fit 1D histograms and generate pedestal tables
  root4star -b -q -l fitAdc4Ped.C > log2 
  echo "Rnnnpedfit.hist.root file complete"
  mv log2 $out
  
  #move all output to single folder
  namefit="fit"
  mv Rnnnped.hist.root $out/R$run.hist.root
  mv Rnnnpedfit.hist.root $out/R$run$namefit.hist.root
  mv ped.sector* $out
  mv Mpt.log $out
  mv T.log $out
  mv $out day$day/outPedR$run

done
