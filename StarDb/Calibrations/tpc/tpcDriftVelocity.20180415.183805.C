TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 105034
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54357; // +/- 8.87238e-06 cm/us All: East = -0.0665899 +/- 0.0021644
  row.laserDriftVelocityWest	 =   5.54357; // +/- 8.87238e-06 cm/us All: West = 0.203349 +/- 0.00232676
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54357 +/- 8.87238e-06
  return (TDataSet *)tableSet;// West = 5.5428 +/- 1.28981e-05 East = 5.54426 +/- 1.22239e-05
};
