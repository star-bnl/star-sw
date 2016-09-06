/*
  root.exe st_physics_adc_17126033_raw_5500003_heed_PhysicsOff_1_4000.ADCTanL.root  AdcTpcTCorrections.C
 */
void AdcTanLTpcTCorrections() {
  gROOT->LoadMacro("TpcT.C+");
  AdcTanLCorr();
}
