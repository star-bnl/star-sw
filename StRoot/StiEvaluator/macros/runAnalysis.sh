#!/bin/bash
listname=$1
i=1
for datafile in $(cat $listname); do
    echo $datafile
    root4star -b -q "StRoot/StiEvaluator/macros/FillHistos.C(\"$datafile\",\"test_`basename $datafile evts.minimc.root`.root\")"
    i=$[$i+1]
done
hadd `basename $listname list`root test_*.root;
rm test_*.root;