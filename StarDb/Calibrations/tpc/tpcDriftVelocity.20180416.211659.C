TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 106037
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55562; // +/- 1.21343e-05 cm/us All: East = -0.156527 +/- 0.00324058
  row.laserDriftVelocityWest	 =   5.55562; // +/- 1.21343e-05 cm/us All: West = 0.173901 +/- 0.00294752
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55562 +/- 1.21343e-05
  return (TDataSet *)tableSet;// West = 5.5548 +/- 1.64896e-05 East = 5.5566 +/- 1.79206e-05
};
