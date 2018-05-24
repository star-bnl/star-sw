TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 135005
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5455; // +/- 0.000106477 cm/us All: East = -1.92751 +/- 6.37277
  row.laserDriftVelocityWest	 =   5.5455; // +/- 0.000106477 cm/us All: West = 0.111016 +/- 0.0268413
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5455 +/- 0.000106477
  return (TDataSet *)tableSet;// West = 5.54549 +/- 0.000106672 East = 5.54855 +/- 0.00175968
};
