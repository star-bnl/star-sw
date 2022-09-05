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
#foreach y (2019 2020 2021) 
#   hadd COLGeV_${y}.root [0-9]*GeV_${y}.root >& COLGeV_${y}.log &
#   hadd FXT_${y}.root [0-9]*get_${y}.root >& FXT_${y}.log &
#end
