#! /bin/tcsh -f
foreach f (`ls -1d znptMax_*.root`)
  foreach k (1 3 5 7) 
    root.exe -q -b lBicshel.C ${f} 'dEdxFit.C+("zenpL","GEX","rqm",-1,-1,'$k')' >& zenpLGEX${k}_${f}.log &
    root.exe -q -b lBicshel.C ${f} 'dEdxFit.C+("zenpL","GEX","rqmB2",-1,-1,'$k')' >& zenpLGEX${k}B2_${f}.log &
    root.exe -q -b lBicshel.C ${f} 'dEdxFit.C+("zenpL","GEX","rqmB4",-1,-1,'$k')' >& zenpLGEX${k}B4_${f}.log &
    root.exe -q -b lBicshel.C ${f} 'dEdxFit.C+("zenpL","GEX","rqmB5",-1,-1,'$k')' >& zenpLGEX${k}B5_${f}.log &
  end
end
