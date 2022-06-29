#! /usr//bin/env tcsh
  foreach p (muon electron pion kaon proton deuteron triton He3 alpha HE6 Li5 Li6 Li7 Be7 Be9 Be10 B11)
#     echo "${p}"
     if (-r ${p}Adc.root) continue;
     root.exe -q -b 'AdcTpcT.C("'../${p}*20.root'","'${p}Adc.root'")' >& ${p}.log &
  end
root.exe -q -b 'AdcTpcT.C("../kaon*20.root","kaonAdc.root")'          >& kaon.log &
root.exe -q -b 'AdcTpcT.C("../proton*20.root","protonAdc.root")'      >& proton.log  &
root.exe -q -b 'AdcTpcT.C("../deuteron*20.root","deuteronAdc.root")'  >& deuteron.log &
root.exe -q -b 'AdcTpcT.C("../triton*20.root","tritonAdc.root")'	     >& triton.log &
root.exe -q -b 'AdcTpcT.C("../He3*20.root","He3Adc.root")'	     >& He3.log &
root.exe -q -b 'AdcTpcT.C("../alpha*20.root","alphaAdc.root")'	     >& alpha.log &
root.exe -q -b 'AdcTpcT.C("../HE6*20.root","HE6Adc.root")'	     >&HE6.log &
root.exe -q -b 'AdcTpcT.C("../Li5*20.root","Li5Adc.root")'	     >&Li5.log &
root.exe -q -b 'AdcTpcT.C("../Li6*20.root","Li6Adc.root")'	     >&Li6.log &
root.exe -q -b 'AdcTpcT.C("../Li7*20.root","Li7Adc.root")'	     >&Li7.log &
root.exe -q -b 'AdcTpcT.C("../Be7*20.root","Be7Adc.root")'	     >&Be7.log &
root.exe -q -b 'AdcTpcT.C("../Be9*20.root","Be9Adc.root")'	     >&Be9.log &
root.exe -q -b 'AdcTpcT.C("../Be10*20.root","Be10Adc.root")'	     >&Be10.log &
root.exe -q -b 'AdcTpcT.C("../B11*20.root","B11Adc.root")'	     >&B11.log &

root.exe -q -b 'AdcTpcT.C("../electron*20.root","electronAdc.root")'	     >&electron.log &
root.exe -q -b 'AdcTpcT.C("../muon*20.root","muonAdc.root")'	     >&muon.log &
