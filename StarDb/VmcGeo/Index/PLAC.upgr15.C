TDataSet *CreateTable() {
  if (!gROOT->GetClass("StarVMCDetector")) return 0;
  Int_t NVmax[2] = {2,30};
  StarVMCDetector *Set = new StarVMCDetector("PLAC");
  Set->SetNVmax(2, NVmax);
  Set->SetIds();
  Set->SetFMT("/HALL_1/CAVE_1/TpcRefSys_1/PXMO_1/PXLA_%d/PLMI_%d/PLAC_1");
  return (TDataSet *)Set;
}
