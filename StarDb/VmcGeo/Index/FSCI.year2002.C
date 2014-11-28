TDataSet *CreateTable() {
  if (!gROOT->GetClass("StarVMCDetector")) return 0;
  Int_t NVmax[4] = {8,3,4,4};
  StarVMCDetector *Set = new StarVMCDetector("FSCI");
  Set->SetNVmax(4, NVmax);
  Set->SetIds();
  Set->SetFMT("/HALL_1/CAVE_1/FPDM_1/FMSS_1/FMOD_1/FPSE_%d/FTOW_%d/FPER_%d/FTAR_%d/FSCI_1");
  return (TDataSet *)Set;
}
