/*
  root.exe gstar_heed_PhysicsOff_triton:9_1_1000.ADC.root AdcTpcTCorrections.C
 */
void AdcTpcTCorrections() {
  gROOT->LoadMacro("TpcT.C+");
  AdcCorrections();
}
