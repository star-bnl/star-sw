#! /bin/tcsh -f
cd ~/bin/; onl CURRENT | SortRun.pl | tee RunXXDefs.pm
git diff .
git ci -m "Update" .
cd ~/reco/2020/TFG19m/RF/11p5GeV.B
CreateRunDirs.pl
foreach d (`ls -1d */2*`)
  cd $d;
  if (-r Done) then
    cd -
    continue
  endif
  daq_2020dR.pl
  if ($?) then
    ls -1d *bla.root
    if ($?) then
      rm sched* *xml;
      touch Done
    endif
    cd -;
    continue;
  endif
  lsf63 ~/xml/daq_2020StiCA.Minuit.TFG19m.xml
  cd -
end
