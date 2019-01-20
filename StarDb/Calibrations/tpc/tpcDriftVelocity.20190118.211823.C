TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 18045
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55581; // +/- 7.93901e-06 cm/us All: East = -0.0274379 +/- 0.00324139
  row.laserDriftVelocityWest	 =   5.55581; // +/- 7.93901e-06 cm/us All: West = 0.493604 +/- 0.00158072
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55581 +/- 7.93901e-06
  return (TDataSet *)tableSet;// West = 5.55487 +/- 8.83831e-06 East = 5.55973 +/- 1.80644e-05
};
