TDataSet *CreateTable() {
  if (!gROOT->GetClass("StarVMCDetector")) return 0;
  Int_t NVmax[2] = {4,238};
  StarVMCDetector *Set = new StarVMCDetector("FPCT");
  Set->SetNVmax(2, NVmax);
  //  Set->SetFMT("/HALL_1/CAVE_1/FPDM_1/FLGD_%d/FLGT_%d/FPCT_1");
  Set->SetFMT("/HALL_1/CAVE_1/FBOX_%d/FTOW_%d/FPCT_1");
  return (TDataSet *)Set;
}
