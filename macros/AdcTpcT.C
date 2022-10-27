/* 
  foreach p (muon electron pion kaon proton deuteron triton He3 alpha HE6 Li5 Li6 Li7 Be7 Be9 Be10 B11)
     echo "${p}"
     if (-r ${p}Adc.root) continue;
     root.exe -q -b 'AdcTpcT.C("../../'${p}'*0.root","'${p}'Adc.root")' >& ${p}.log &
  end
 */
void AdcTpcT(const Char_t *files="*.root", const Char_t *Out = "") {
#if defined(__CINT__) && ! defined(__MAKECINT__)
  gROOT->LoadMacro("TpcT.C+");
  TpcTAdc(files,Out);
#else
  cout << "Please don't run it withc AClick" << endl;
#endif
}
