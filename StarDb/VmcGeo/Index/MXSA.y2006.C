TDataSet *CreateTable() {
  if (!gROOT->GetClass("StarVMCDetector")) return 0;
  Int_t NVmax[2] = {2,2};
  StarVMCDetector *Set = new StarVMCDetector("MXSA");
  Set->SetNVmax(2, NVmax);
  Set->SetIds();
  Set->SetFMT("/HALL_1/CAVE_1/MUTD_1/MUSC_1/MTRA_%d/MXTR_1/MMTC_1/MXSA_%d");
  return (TDataSet *)Set;
}
