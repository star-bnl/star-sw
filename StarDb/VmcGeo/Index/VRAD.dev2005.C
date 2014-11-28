TDataSet *CreateTable() {
  if (!gROOT->GetClass("StarVMCDetector")) return 0;
  Int_t NVmax[2] = {2,3};
  Int_t Ids[6] = {
	1001,1002,1003,2001,2002,2003};
  StarVMCDetector *Set = new StarVMCDetector("VRAD");
  Set->SetNVmax(2, NVmax);
  Set->SetIds(6, Ids);
  Set->SetFMT("/HALL_1/CAVE_1/VPDD_%d/VRNG_1/VSEC_%d/VDET_1/VDTI_1/VCNV_1/VRAD_1");
  return (TDataSet *)Set;
}
