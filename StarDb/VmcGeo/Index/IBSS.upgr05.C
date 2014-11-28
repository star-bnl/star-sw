TDataSet *CreateTable() {
  if (!gROOT->GetClass("StarVMCDetector")) return 0;
  Int_t NVmax[4] = {2,27,13,2};
  StarVMCDetector *Set = new StarVMCDetector("IBSS");
  Set->SetNVmax(4, NVmax);
  Set->SetIds();
  Set->SetFMT("/HALL_1/CAVE_1/TpcRefSys_1/IBMO_1/IBMY_%d/IBAM_%d/IBLM_%d/IBSS_%d");
  return (TDataSet *)Set;
}
