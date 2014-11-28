TDataSet *CreateTable() {
  if (!gROOT->GetClass("StarVMCDetector")) return 0;
  Int_t NVmax[3] = {    4,  20, 16};
  Int_t NVp10[3] = {10000,   1,100};
  StarVMCDetector *Set = new StarVMCDetector("sfsd");
  Set->SetVolIdOffset(7000);
  Set->SetNVmax(3, NVmax);
  Set->SetNVp10(3, NVp10);
  Set->SetFMT("/HALL_1/CAVE_1/TpcRefSys_1/SVTT_1/SFMO_1/SsdSector_%d/SFLM_%d/SFDM_1/SFSW_%d/SFSL_1/sfsd_1");
  return (TDataSet *)Set;
}
