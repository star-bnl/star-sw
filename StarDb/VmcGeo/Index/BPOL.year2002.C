TDataSet *CreateTable() {
  if (!gROOT->GetClass("StarVMCDetector")) return 0;
  Int_t NVmax[4] = {2,2,6,3};
  StarVMCDetector *Set = new StarVMCDetector("BPOL");
  Set->SetNVmax(4, NVmax);
  Set->SetIds();
  Set->SetFMT("/HALL_1/CAVE_1/BBC_1/BBCM_%d/BBCA_%d/THXM_%d/SHXT_%d/BPOL_1");
  return (TDataSet *)Set;
}
