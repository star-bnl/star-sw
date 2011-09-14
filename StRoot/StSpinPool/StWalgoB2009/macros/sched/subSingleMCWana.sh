#!/bin/sh
mxEve=500000
codePath=/star/u/stevens4/wAnalysis/efficXsec/
inpPath=/star/u/stevens4/wAnalysis/efficXsec/fillListE/sl11b/removeBadFiles/
outPath=/star/u/stevens4/wAnalysis/efficXsec/outEmb/

echo outPath=$outPath= inpPath=$inpPath=

for fillNo in Wplus0 Wplus1 Wplus2 Wplus3 Wplus4 Wplus5 Wplus6 Wplus7 Wplus8 Wplus9 Wminus0 Wminus1 Wminus2 Wminus3 Wminus4 Wminus5 Wminus6 Wminus7 Wminus8 Wminus9; do 
    isMC=350

    star-submit-template -template singleMCWanaJobTempl.xml -entities  n1=$mxEve,outPath=$outPath,codePath=$codePath,inpPath=$inpPath,fillNo=$fillNo,isMC=$isMC
done

for fillNo in Wtau0 Wtau1 Wtau2 Wtau3 Wtau4 Wtau5 Wtau6 Wtau7 Wtau8 Wtau9 ; do 
    isMC=351

    star-submit-template -template singleMCWanaJobTempl.xml -entities  n1=$mxEve,outPath=$outPath,codePath=$codePath,inpPath=$inpPath,fillNo=$fillNo,isMC=$isMC
done

for fillNo in Ze+e-Interf0 Ze+e-Interf1 Ze+e-Interf2 Ze+e-Interf3 Ze+e-Interf4 Ze+e-Interf5 Ze+e-Interf6 Ze+e-Interf7 Ze+e-Interf8 Ze+e-Interf9 Zany0 Zany1 Zany2 Zany3 Zany4 Zany5 Zany6 Zany7 Zany8 Zany9 ; do 
    isMC=352

    star-submit-template -template singleMCWanaJobTempl.xml -entities  n1=$mxEve,outPath=$outPath,codePath=$codePath,inpPath=$inpPath,fillNo=$fillNo,isMC=$isMC
done

 
