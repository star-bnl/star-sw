TDataSet *CreateTable() {
  if (!gROOT->GetClass("StarVMCDetector")) return 0;
  Int_t NVmax[1] = {2};
  StarVMCDetector *Set = new StarVMCDetector("OQUA");
  Set->SetNVmax(1, NVmax);
  Set->SetIds();
  Set->SetFMT("/HALL_1/CAVE_1/CALB_1/RICH_1/SRIC_1/OQUA_%d");
  return (TDataSet *)Set;
}
