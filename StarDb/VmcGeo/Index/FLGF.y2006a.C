TDataSet *CreateTable() {
  if (!gROOT->GetClass("StarVMCDetector")) return 0;
  Int_t NVmax[2] = {6,180};
  StarVMCDetector *Set = new StarVMCDetector("FLGF");
  Set->SetNVmax(2, NVmax);
  Set->SetIds();
  Set->SetFMT("/HALL_1/CAVE_1/FBOX_%d/FLGF_%d");
  return (TDataSet *)Set;
}
