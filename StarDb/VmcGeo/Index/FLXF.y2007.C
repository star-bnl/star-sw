TDataSet *CreateTable() {
  if (!gROOT->GetClass("StarVMCDetector")) return 0;
  Int_t NVmax[2] = {4,394};
  StarVMCDetector *Set = new StarVMCDetector("FLXF");
  Set->SetNVmax(2, NVmax);
  Set->SetIds();
  Set->SetFMT("/HALL_1/CAVE_1/FBOX_%d/FLXF_%d");
  return (TDataSet *)Set;
}
