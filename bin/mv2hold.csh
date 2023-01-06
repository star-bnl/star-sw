#! /bin/tcsh -f
#set D = ~/work/Histograms/RunXIX_XXII_78
set D = ~/work/Histograms/RunXX210
#set T = TpcSecRowB; 
#set T = TpcZCorrectionC;  
#set T = TpcEtaCorrectionB; 
set T = TpcLengthCorrectionMDN;
set H = hold.`basename ${D}`
echo "D = ${D}, T = ${T}, H = ${H}"
if (! -d ${H}) mkdir ${H}
set ext  = "C";
echo "${T}" | grep TpcSecRowB
if (! $?) set ext = "root"
foreach f (`ls -1d ${D}/${T}.*.${ext}`)
  set b = `basename ${f}`;
  if (-r ${b}) mv ${b} ${H}
  cp ${f} .
end
