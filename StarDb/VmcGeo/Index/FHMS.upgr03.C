TDataSet *CreateTable() {
  if (!gROOT->GetClass("StarVMCDetector")) return 0;
  Int_t NVmax[2] = {2,100};
  StarVMCDetector *Set = new StarVMCDetector("FHMS");
  Set->SetNVmax(2, NVmax);
  Set->SetIds();
  Set->SetFMT("/HALL_1/CAVE_1/FPDM_1/FMSS_1/FMOD_1/FSHM_1/FXSG_%d/FHMS_%d");
  return (TDataSet *)Set;
}
