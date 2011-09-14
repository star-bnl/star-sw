#!/bin/sh
mxEve=500000
codePath=/star/u/stevens4/wAnalysis/xSecPaper/
inpPath=/star/u/stevens4/wAnalysis/xSecPaper/lists/sl11b
outPath=/star/data01/pwg/stevens4/wAnalysis/xSecPaper/sl11b/

echo outPath=$outPath= inpPath=$inpPath=

star-submit-template -template multiWanaJobTempl.xml -entities  n1=$mxEve,outPath=$outPath,codePath=$codePath,inpPath=$inpPath

 
