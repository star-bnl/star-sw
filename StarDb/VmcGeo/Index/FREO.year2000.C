TDataSet *CreateTable() {
  if (!gROOT->GetClass("StarVMCDetector")) return 0;
  Int_t NVmax[1] = {2};
  StarVMCDetector *Set = new StarVMCDetector("FREO");
  Set->SetNVmax(1, NVmax);
  Set->SetVolIdOffset(4000);
  Set->SetFMT("/HALL_1/CAVE_1/CALB_1/RICH_1/SRIC_1/OQUF_%d/FREO_1");
  return (TDataSet *)Set;
}
