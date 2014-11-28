TDataSet *CreateTable() {
  if (!gROOT->GetClass("StarVMCDetector")) return 0;
  Int_t NVmax[2] = {2,19};
  Int_t Ids[38] = {
	1001,1002,1003,1004,1005,1006,1007,1008,1009,1010,1011,1012,1013,1014,1015,1016,1017,1018,1019,
	2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019};
  StarVMCDetector *Set = new StarVMCDetector("VRAD");
  Set->SetNVmax(2, NVmax);
  Set->SetIds(38, Ids);
  Set->SetFMT("/HALL_1/CAVE_1/VPDD_%d/VRNG_1/VDET_%d/VDTI_1/VCNV_1/VRAD_1");
  return (TDataSet *)Set;
}
