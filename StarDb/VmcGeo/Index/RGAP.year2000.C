TDataSet *CreateTable() {
  if (!gROOT->GetClass("StarVMCDetector")) return 0;
  Int_t NVmax[1] = {30}; // guess
  StarVMCDetector *Set = new StarVMCDetector("RGAP");
  Set->SetNVmax(1, NVmax);
  Set->SetVolIdOffset(1000);
  Set->SetFMT("/HALL_1/CAVE_1/CALB_1/RICH_1/SRIC_1/RGAP_%d");
  return (TDataSet *)Set;
}
