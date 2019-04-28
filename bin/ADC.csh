#! /usr//bin/env tcsh
FPE_OFF
ln -s ~/macros/.sl* .
foreach io (I O) 
    foreach k (Z TanL dXLog npads ntmbks npadtmbks)
      set f = ${io}3D${k}
      root.exe -q -b adc.root lBichsel.C 'dEdxFit.C+("'${f}'","ADC")' >& ${f}.log &
    end
end
# eod 
