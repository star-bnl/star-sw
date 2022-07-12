#! /bin/tcsh -f
@ count = 0
#foreach f (`ls -1d SecRow3*G4E3p85GeV_fixedTarget_2021.csh`)
foreach f (`ls -1d *G4E*.csh`)
  set d = `basename ${f} .csh`
  set root = ${d}.root;  
  if (-r $root) continue;  
    echo "${f}"
#  csh -x ${f} >& ${f}.log &  
  @ count++;  echo "count $count => $root";
  if ($count > 20) break
end
