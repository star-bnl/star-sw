TDataSet *CreateTable() {
  if (!gROOT->GetClass("StarVMCDetector")) return 0;
  Int_t NVmax[1] = {4};
  StarVMCDetector *Set = new StarVMCDetector("IGAL");
  Set->SetNVmax(1, NVmax);
  Set->SetIds();
  Set->SetFMT("/HALL_1/CAVE_1/IGMO_1/IGDO_%d/IGAL_1");
  return (TDataSet *)Set;
}
