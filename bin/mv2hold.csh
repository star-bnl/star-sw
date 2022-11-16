#! /bin/tcsh -f
set D = ~/work/Histograms/RunXIX_XXII_51
set T = TpcSecRowB;  # TpcZCorrectionC
set H = hold.`basename ${D}`
echo "D = ${D}, T = ${T}, H = ${H}"
if (! -d ${H}) mkdir ${H}
foreach f (`ls -1d ${D}/${T}.*.root`)
#foreach f (`ls -1d ${D}/${T}.*.C`)
  set b = `basename ${f}`;
  if (-r ${b}) mv ${b} ${H}
  cp ${f} .
end
