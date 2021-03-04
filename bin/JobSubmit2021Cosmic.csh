#! /bin/tcsh -f
cd ~/bin/; onl CURRENT | SortRun.pl | tee RunXXIDefs.pm
git diff .
git ci -m "Update" .
#cd ~/reco/2020/TFG19m/RF/11p5GeV.B
#cd CreateRunDirs.pl/net/l401/data/scratch2/reco/2020/TFG20a/RF/31p2GeV_fixedTarget
#cd /net/l401/data/scratch2/reco/2020/TFG20a/RF/9p8GeV_fixedTarget
#cd /net/l401/data/scratch2/reco/2020/TFG20a/RF/9p2GeV
#cd /net/l401/data/scratch2/reco/2020/TFG20a/RF/9p2GeVb
foreach D (`ls -d /hlt/cephfs/reco/2021/RF/TFG21c/Cosmic*`)
#foreach D (`ls -d /hlt/cephfs/reco/2021/FF/TFG21c.B/Cosmic*`)
  cd ${D}
  CreateRunDirs.pl
  foreach d (`ls -1d ???/2*`)
    cd $d;
    if (-r Done || -r Submitted) then
      cd -
     continue
    endif
    daq_2021dR.pl
    if ($?) then
      ls -1d *bla.root
      if ($?) then
        rm sched* *xml;
        touch Done
      endif
      cd -;
      continue;
    endif
    /net/l402/data/fisyak/STAR/packages/.DEV2/scripts/star-submit -p bnl_condor_online_CpuModelNumber6X ~/xml/daq_2021_Cosmics.xml
    touch Submitted
    cd -
  end
end
#  /net/l402/data/fisyak/STAR/packages/.DEV2/scripts/star-submit -p bnl_condor_online_CpuModelNumber63 ~/xml/daq_2021StiCA.Minuit.TFG19m.xml
#  /net/l402/data/fisyak/STAR/packages/.DEV2/scripts/star-submit -p bnl_condor_online_CpuModelNumber6X ~/xml/daq_2021StiCA.Minuit.TFG20a.xml
#  /net/l402/data/fisyak/STAR/packages/.DEV2/scripts/star-submit -p bnl_condor_online_CpuModelNumber6X ~/xml/daq_2021.xml
