#! /usr//bin/env tcsh
FPE_OFF
#set list = `ls -1d  /net/l401/data/scratch1/reco/2020/TFG19m/RF/11p5GeV/3??`
set list = `ls -1d /net/l401/data/scratch1/reco/2020/TFG19m/RF/11p5GeV.B/3??/*/*picoDst.root | awk -Fhlt '{print $1}' | sort -u`
foreach d (${list})
  set day = `basename ${d}`
#  root.exe -q -b 'kfpAnalysis.C(1000000,"'${d}'/*/*picoDst.root","picoAna2011AuAu27_'${day}'.root","y2020")' >& picoAna2011AuAu27_${day}.log &
  root.exe -q -b 'kfpAnalysis.C(1000000,"'${d}'/*picoDst.root","picoAna2011AuAu27_'${day}'.root","y2020")' >& picoAna2011AuAu27_${day}.log &
#  root.exe -q -b 'kfpAnalysis.C(1000000,"'${d}'/*/*MuDst.root","MuAna2011AuAu27_'${day}'.root","y2020")' >& picoAna2011AuAu27_${day}.log &
end
