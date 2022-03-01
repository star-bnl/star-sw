#! /bin/tcsh -f
set D = `basename ${PWD}`
set dist = ~/work/Histograms/${D}
if (! -d ${dist}) mkdir ${dist}
foreach d (`ls -1d *GeV*`)
  if (-r ${dist}/${d}.root) continue;
  cd ${d}
  ls -1d All*_2.root
  if (! $?) then
    hadd ${dist}/${d}.root All*.root >& hadd.log &
  else
    mv All*_1.root ${dist}/${d}.root
  endif
  cd -;
end
