TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 104006
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54338; // +/- 1.16943e-05 cm/us All: East = -999 +/- 999
  row.laserDriftVelocityWest	 =   5.54338; // +/- 1.16943e-05 cm/us All: West = 0.17265 +/- 0.00210833
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54338 +/- 1.16943e-05
  return (TDataSet *)tableSet;// West = 5.54338 +/- 1.16943e-05 East = -999 +/- 999
};
