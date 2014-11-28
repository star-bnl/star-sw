TDataSet *CreateTable() {
  if (!gROOT->GetClass("StarVMCDetector")) return 0;
  Int_t NVmax[1] = {6};
  StarVMCDetector *Set = new StarVMCDetector("FGSC");
  Set->SetNVmax(1, NVmax);
  Set->SetIds();
  Set->SetFMT("/HALL_1/CAVE_1/TpcRefSys_1/FGMO_1/FGDO_%d/FGSC_1");
  return (TDataSet *)Set;
}
