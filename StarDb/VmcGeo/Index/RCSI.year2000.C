TDataSet *CreateTable() {
  if (!gROOT->GetClass("StarVMCDetector")) return 0;
  Int_t NVmax[1] = {4};
  StarVMCDetector *Set = new StarVMCDetector("RCSI");
  Set->SetNVmax(1, NVmax);
  Set->SetVolIdOffset(2000);
  Set->SetFMT("/HALL_1/CAVE_1/CALB_1/RICH_1/SRIC_1/ALMF_1/HOLE_%d/RCSI_1");
  return (TDataSet *)Set;
}
