TDataSet *CreateTable() {
  if (!gROOT->GetClass("StarVMCDetector")) return 0;
  Int_t NVmax[3] = { 24, 2,45};
  Int_t NVp10[3] = {100, 0, 1};
  StarVMCDetector *Set = new StarVMCDetector("tpad");
  Set->SetNVmax(3, NVmax);
  Set->SetNVp10(3, NVp10);
  Set->SetFMT("/HALL_1/CAVE_1/TpcRefSys_1/TPCE_1/TpcSectorWhole_%d/TpcGas_1/TpcPadPlane_%d/tpad_%d");
  return (TDataSet *)Set;
}
