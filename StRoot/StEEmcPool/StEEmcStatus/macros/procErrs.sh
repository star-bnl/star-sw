#!/bin/bash

wrkdir=..
inpO=$wrkdir/day
outP=.

ls='*.errs'
for file in $ls
do
    name=${file:0:9}
    echo $name
    day=${file:3:3}
    echo $day
    inpP=$inpO$day
    inp=$inpP/outPed$name/
    out=$outP/$name/
    echo $inp
    echo $out
                                       
    mkdir  $name
    cp -rp $inp/ped.* $out

    mv $file $name/$file
    mv $name.log $name/$name.log
    cp DistrStat2Sectors.C $name/
    cd $name
    root -q -b -l DistrStat2Sectors.C\(\"$file\"\)
	rm Distr*.C
	cd ..

done
