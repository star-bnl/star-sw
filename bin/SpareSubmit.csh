@ count = 0
@ i     = 1
while ( $i < 3)     
  root.exe -q -b AdcSparseD2.root lBichsel.C 'dEdxFit.C+("Sparse","GP","RQ",'$i')' >& adcFit${i}.log & 
    @ i++;
end
