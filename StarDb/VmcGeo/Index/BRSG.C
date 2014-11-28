TDataSet *CreateTable() {
  if (!gROOT->GetClass("StarVMCDetector")) return 0;
  Int_t NVmax[4] = {2,33,32,6};
  StarVMCDetector *Set = new StarVMCDetector("BRSG");
  Set->SetNVmax(4, NVmax);
  Set->SetFMT("/HALL_1/CAVE_1/TpcRefSys_1/BTOF_1/BTOH_%d/BSEC_%d/BTRA_1/BXTR_1/BRTC_1/BRMD_%d/BRDT_1/BRSG_%d");
  return (TDataSet *)Set;
}
