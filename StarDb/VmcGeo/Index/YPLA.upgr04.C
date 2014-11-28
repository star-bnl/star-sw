TDataSet *CreateTable() {
  if (!gROOT->GetClass("StarVMCDetector")) return 0;
  Int_t NVmax[2] = {48,20};
  StarVMCDetector *Set = new StarVMCDetector("YPLA");
  Set->SetNVmax(2, NVmax);
  Set->SetIds();
  Set->SetFMT("/HALL_1/CAVE_1/TpcRefSys_1/YPXM_1/YPLM_%d/YPWP_1/YPLA_%d");
  return (TDataSet *)Set;
}
