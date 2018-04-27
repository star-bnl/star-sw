TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 86018
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.51938; // +/- 7.69484e-06 cm/us All: East = -0.582701 +/- 0.00492333
  row.laserDriftVelocityWest	 =   5.51938; // +/- 7.69484e-06 cm/us All: West = 0.20466 +/- 0.00143831
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.51938 +/- 7.69484e-06
  return (TDataSet *)tableSet;// West = 5.51904 +/- 8.00813e-06 East = 5.52338 +/- 2.77822e-05
};
