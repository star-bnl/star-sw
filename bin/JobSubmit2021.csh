#! /bin/tcsh -f
#cd ~/bin/; onl CURRENT | SortRun.pl | tee RunXIXDefs.pm
#git diff .
#git ci -m "Update" RunXIXDefs.pm
#cd ~/reco/2020/TFG19m/RF/11p5GeV.B
#cd CreateRunDirs.pl/net/l401/data/scratch2/reco/2020/TFG20a/RF/31p2GeV_fixedTarget
#cd /net/l401/data/scratch2/reco/2020/TFG20a/RF/9p8GeV_fixedTarget
#cd /net/l401/data/scratch2/reco/2020/TFG20a/RF/9p2GeV
#cd /net/l401/data/scratch2/reco/2020/TFG20a/RF/9p2GeVb
#cd /hlt/cephfs/reco/2021/RF/DEV2/7p7GeV_2021.C
#cd /hlt/cephfs/reco/2021/RF/TFG21c.B/7p7GeV_2021
#cd /hlt/cephfs/reco/2021/FF/TFG21c.B/7p7GeV_2021
#cd /hlt/cephfs/reco/2021/RF/TFG21h/7p7GeV_2021
CreateRunDirs2021.pl
foreach d (`ls -1d ???/2*`)
  cd $d;
  if (-r Done || -r Submitted) then
    cd -
    continue
  endif
  daq_2021dR.pl
#  daq_2021dR2.pl
  if ($?) then
    ls -1d *bla.root
    if ($?) then
      rm sched* *xml fisyak.log;
      touch Done
    endif
    cd -;
    continue;
  endif
#  set run = `basename ${d}`;  
#  if (${run} > 22160013 && ${run} < 22161002) then
#  /net/l402/data/fisyak/STAR/packages/.DEV2/scripts/star-submit -p bnl_condor_online_CpuModelNumber63 ~/xml/daq_2021StiCA.Minuit.TFG19m.xml
#  /net/l402/data/fisyak/STAR/packages/.DEV2/scripts/star-submit -p bnl_condor_online_CpuModelNumber6X ~/xml/daq_2021StiCA.Minuit.TFG20a.xml
#  /net/l402/data/fisyak/STAR/packages/.DEV2/scripts/star-submit -p bnl_condor_online_CpuModelNumber6X ~/xml/daq_2021_Cosmics.xml
#  /net/l402/data/fisyak/STAR/packages/.DEV2/scripts/star-submit -p bnl_condor_online_CpuModelNumber6X ~/xml/daq_2021.TFG21c.xml
#  /net/l402/data/fisyak/STAR/packages/.DEV2/scripts/star-submit -p bnl_condor_online_CpuModelNumber6X ~/xml/daq_2019.TFG22b.xml
#  /net/l402/data/fisyak/STAR/packages/.DEV2/scripts/star-submit -p bnl_condor_online_CpuModelNumber6X ~/xml/daq_2021.TFG22b.xml
#  /net/l402/data/fisyak/STAR/packages/.DEV2/scripts/star-submit -p bnl_condor_online_CpuModelNumber6X ~/xml/daq_2021.DEV2.xml
  /net/l402/data/fisyak/STAR/packages/.DEV2/scripts/star-submit -p bnl_condor_online_CpuModelNumber6X ~/xml/daq_2021.TFG23a.xml
#  /net/l402/data/fisyak/STAR/packages/.DEV2/scripts/star-submit -p bnl_condor_online_CpuModelNumber6X ~/xml/daq_2021.TFG22g.xml
#  endif
  touch Submitted
  cd -
end
#================================= Summurizing  ===============================================
@ count = 0
foreach done (`ls -1d ???/*/Done`)
  set d = `dirname ${done}`
  cd ${d}; pwd;
  ls -ltr | tail -1 | grep Chain	
  if ($?) then	
    rm -rf .sl*
    ln -s ~/macros/.sl73_* .	
    root.exe -q -b 'Chain.C+("./*picoDst.root","PicoDst")' >&  Chain.log  &
    @ count++;  echo "count $count";
    if ($count > 120) then 
        cd -;
        break;
    endif
  endif
  cd -;
end
if ($count > 0) then
  sleep 60; 
  touch `grep total ???/2*/Chain.log | awk 'BEGIN{n= 0; s = 0}{n += $6; s += $10}END{printf("%7.3fM_%fGB\n", n/1.e6,s)}'`
endif
