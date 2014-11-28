TDataSet *CreateTable() {
  if (!gROOT->GetClass("StarVMCDetector")) return 0;
  Int_t NVmax[3] = {4,21,2};
  StarVMCDetector *Set = new StarVMCDetector("FDSW");
  Set->SetNVmax(3, NVmax);
  Set->SetIds();
  Set->SetFMT("/HALL_1/CAVE_1/FSMO_1/FDMO_%d/FDMS_%d/FDMW_1/FDSW_%d");
  return (TDataSet *)Set;
}
