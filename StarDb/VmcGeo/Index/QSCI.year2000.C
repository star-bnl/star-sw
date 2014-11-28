TDataSet *CreateTable() {
  if (!gROOT->GetClass("StarVMCDetector")) return 0;
  Int_t NVmax[2] = {2,260};
  StarVMCDetector *Set = new StarVMCDetector("QSCI");
  Set->SetNVmax(2, NVmax);
  Set->SetIds();
  Set->SetFMT("/HALL_1/CAVE_1/ZCAL_%d/QCAL_1/QDIV_%d/QSCI_1");
  return (TDataSet *)Set;
}
