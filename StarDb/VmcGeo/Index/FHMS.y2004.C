TDataSet *CreateTable() {
  if (!gROOT->GetClass("StarVMCDetector")) return 0;
  Int_t NVmax[2] = {6,100};
  StarVMCDetector *Set = new StarVMCDetector("FHMS");
  Set->SetNVmax(2, NVmax);
  Set->SetIds();
  Set->SetFMT("/HALL_1/CAVE_1/FBOX_%d/FSHM_1/FHMS_%d");
  return (TDataSet *)Set;
}
