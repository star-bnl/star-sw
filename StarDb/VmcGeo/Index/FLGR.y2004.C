TDataSet *CreateTable() {
  if (!gROOT->GetClass("StarVMCDetector")) return 0;
  Int_t NVmax[2] = {8,56};
  StarVMCDetector *Set = new StarVMCDetector("FLGR");
  Set->SetNVmax(2, NVmax);
  Set->SetIds();
  Set->SetFMT("/HALL_1/CAVE_1/FBOX_%d/FLGT_%d/FWAL_1/FLGR_1");
  return (TDataSet *)Set;
}
