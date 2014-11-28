TDataSet *CreateTable() {
  if (!gROOT->GetClass("StarVMCDetector")) return 0;
  Int_t NVmax[1] = {30};
  StarVMCDetector *Set = new StarVMCDetector("MMRP");
  Set->SetNVmax(1, NVmax);
  Set->SetFMT("/HALL_1/CAVE_1/MUTD_1/MUSC_%d/MMRP_1");
  return (TDataSet *)Set;
}
