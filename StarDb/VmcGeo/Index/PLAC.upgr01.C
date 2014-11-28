TDataSet *CreateTable() {
  if (!gROOT->GetClass("StarVMCDetector")) return 0;
  Int_t NVmax[2] = {  3,11};
  Int_t NVp10[2] = {100, 1};
  StarVMCDetector *Set = new StarVMCDetector("PLAC");
  Set->SetNVmax(2, NVmax);
  Set->SetNVp10(2, NVp10);
  Set->SetFMT("/HALL_1/CAVE_1/TpcRefSys_1/PXMO_1/PSEC_%d/PLMO_%d/PLAC_1");
  return (TDataSet *)Set;
}
