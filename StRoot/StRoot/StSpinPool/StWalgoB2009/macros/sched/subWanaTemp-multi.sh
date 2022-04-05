#!/bin/sh
mxEve=500000
codePath=/star/u/stevens4/wAnalysis/xSecPaper/gpc/aaa/
inpPath=/star/institutions/iucf/stevens4/freezer/2011-Wana-SL11b/lists/sl11b
outPath=/star/u/stevens4/wAnalysis/xSecPaper/gpc/aaa/
schedMacro=$codePath/StRoot/StSpinPool/StWalgoB2009/macros/sched/

echo outPath=$outPath= inpPath=$inpPath=

mkdir -p $outPath/
mkdir -p $outPath/jets/
mkdir -p $outPath/data/
mkdir -p $outPath/log/
mkdir -p $outPath/stdOut/

star-submit-template -template $schedMacro/multiWanaJobTempl.xml -entities  n1=$mxEve,outPath=$outPath,codePath=$codePath,inpPath=$inpPath

 
