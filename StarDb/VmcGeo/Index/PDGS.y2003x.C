TDataSet *CreateTable() {
  if (!gROOT->GetClass("StarVMCDetector")) return 0;
  Int_t NVmax[5] = {3,7,2,72,72};
  StarVMCDetector *Set = new StarVMCDetector("PDGS");
  Set->SetNVmax(5, NVmax);
  Set->SetIds();
  Set->SetFMT("/HALL_1/CAVE_1/PHMD_1/PHMS_%d/PHSR_%d/PMDA_%d/AIRA_1/PHCA_1/ASTR_%d/PSTR_%d/PDCU_1/PDGS_1");
  return (TDataSet *)Set;
}
