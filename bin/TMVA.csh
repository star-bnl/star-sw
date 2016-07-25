#root.exe -q -b lMuDst.C MuMcPrV.C+  >& MuMcPrV26.log
#root.exe -q -b lMuDst.C 'MuMcPrV.C+(kTRUE)' >& MuMcPrV26TMVA.log
# ppv: root.exe -q -b lMuDst.C 'MuMcPrV.C+(kTRUE,0.250)' >& MuMcPrV27TMVAR.log
# kfv: root.exe -q -b lMuDst.C 'MuMcPrV.C+(kTRUE,0.183)' >& MuMcPrV27TMVAR.log
foreach dir ( KFV+pileup PPV+pileup KFV PPV) 
cd $dir; 
#if (! -r MuMcPrV30.root) then
#if (-r MuMcPrV30.log) rm MuMcPrV30.log
#root.exe -q -b lMuDst.C MuMcPrV.C+  >& MuMcPrV30.log &
#sleep 60; 
#endif
if (! -r MuMcPrV31TMVARank.root) then
echo $dir | grep PPV
set R = 0.0
#set R = 0.183
#if (! $?) set R = 0.250 
if (-r MuMcPrV31TMVAR.log) rm  MuMcPrV31TMVAR.log
root.exe -q -b lMuDst.C 'MuMcPrV.C+(kTRUE,'${R}')' >& MuMcPrV31TMVAR.log &
cd ..
end
# EOD 
