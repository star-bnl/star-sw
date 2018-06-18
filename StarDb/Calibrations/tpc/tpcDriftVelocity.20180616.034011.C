TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 166020
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54676; // +/- 5.85784e-06 cm/us All: East = -0.27108 +/- 0.00403855
  row.laserDriftVelocityWest	 =   5.54676; // +/- 5.85784e-06 cm/us All: West = 0.21752 +/- 0.00108334
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54676 +/- 5.85784e-06
  return (TDataSet *)tableSet;// West = 5.54658 +/- 6.06459e-06 East = 5.54931 +/- 2.26273e-05
};
