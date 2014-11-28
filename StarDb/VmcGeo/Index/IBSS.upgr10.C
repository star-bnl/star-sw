TDataSet *CreateTable() {
  if (!gROOT->GetClass("StarVMCDetector")) return 0;
  Int_t NVmax[3] = {15,9,2};
  StarVMCDetector *Set = new StarVMCDetector("IBSS");
  Set->SetNVmax(3, NVmax);
  Set->SetIds();
  Set->SetFMT("/HALL_1/CAVE_1/TpcRefSys_1/IBMO_1/IBMY_1/IBAM_%d/IBLM_%d/IBSS_%d");
  return (TDataSet *)Set;
}
