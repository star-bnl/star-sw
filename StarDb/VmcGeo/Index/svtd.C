TDataSet *CreateTable() {
  if (!gROOT->GetClass("StarVMCDetector")) return 0;
  Int_t NVmax[4] = {    2,   6,16,  7};
  Int_t NVp10[4] = {10000,1000, 1,100};
  StarVMCDetector *Set = new StarVMCDetector("svtd");
  Set->SetNVmax(4, NVmax);
  Set->SetNVp10(4, NVp10);
  Set->SetFMT("/HALL_1/CAVE_1/TpcRefSys_1/SVTT_1/ClamShell_%d/Layer_%d/Ladder_%d/SLDI_1/svtd_%d");
  return (TDataSet *)Set;
}
