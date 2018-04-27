TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 100005
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53951; // +/- 6.13304e-05 cm/us All: East = -999 +/- 999
  row.laserDriftVelocityWest	 =   5.53951; // +/- 6.13304e-05 cm/us All: West = 0.830511 +/- 0.0177837
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53951 +/- 6.13304e-05
  return (TDataSet *)tableSet;// West = 5.53951 +/- 6.13304e-05 East = -999 +/- 999
};
