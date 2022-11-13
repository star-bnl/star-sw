#! /bin/tcsh -f
set D = ~/work/Histograms/RunXIX_XXII_48
set T = TpcSecRowB
set H = `basename ${D}`
echo "D = ${D}, T = ${T}, H = ${H}"
if (! -d ${H}) mkdir ${H}
foreach f (`ls -1d ${D}/${T}.*.root`)
  set b = `basename ${f}`;
  if (-r ${b}) mv ${b} ${H}
  cp ${f} .
end
