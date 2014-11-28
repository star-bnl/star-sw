TDataSet *CreateTable() {
  if (!gROOT->GetClass("StarVMCDetector")) return 0;
  Int_t NVmax[4] = {   2,12,  2,160};
  Int_t NVp10[4] = {1000, 1,100, 10}; // ???
  StarVMCDetector *Set = new StarVMCDetector("TMSE");
  Set->SetNVmax(4, NVmax);
  Set->SetNVp10(4, NVp10);
  Set->SetFMT("/HALL_1/CAVE_1/TPCE_1/TPEA_%d/TESS_%d/TSEC_%d/TMWC_1/TMEA_1/TMSE_%d");
  return (TDataSet *)Set;
}
