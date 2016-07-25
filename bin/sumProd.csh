#!/bin/env tcsh
set sets=`ls -1 Hist*_*_*.root | awk -F_ '{print $1"_"$2"_"$3}' | sort -u`
foreach s ($sets) 
    echo root.exe -q -b ${s}_*.root \'Hadd.C\(\"../${s}.root\"\)\'
    root.exe -q -b ${s}_*.root Hadd.C\(\"../${s}.root\"\)
end
# e o d
