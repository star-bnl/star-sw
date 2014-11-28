TDataSet *CreateTable() {
  if (!gROOT->GetClass("StarVMCDetector")) return 0;
  Int_t NVmax[2] = {24,12};
  StarVMCDetector *Set = new StarVMCDetector("IBSS");
  Set->SetNVmax(2, NVmax);
  Set->SetIds();
  Set->SetFMT("/HALL_1/CAVE_1/TpcRefSys_1/IBMO_1/IBMY_1/IBAM_%d/IBLM_%d/IBSS_1");
  return (TDataSet *)Set;
}
