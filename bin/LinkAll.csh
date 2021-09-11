#! /bin/tcsh -f
foreach f (`ls -1d ../All*.root`)
  set b = `basename ${f}`; set c = `echo ${b} | sed -e 's/All_//'`; ln -s ${f} ${c}
end
# eod
