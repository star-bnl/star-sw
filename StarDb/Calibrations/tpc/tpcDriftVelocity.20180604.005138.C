TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 154057
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55346; // +/- 1.01338e-05 cm/us All: East = -0.0428261 +/- 0.0041661
  row.laserDriftVelocityWest	 =   5.55346; // +/- 1.01338e-05 cm/us All: West = 0.111021 +/- 0.00197995
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55346 +/- 1.01338e-05
  return (TDataSet *)tableSet;// West = 5.55334 +/- 1.11991e-05 East = 5.55404 +/- 2.38063e-05
};
