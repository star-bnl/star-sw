TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 120035
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53387; // +/- 1.76426e-05 cm/us All: East = -0.754035 +/- 0.00780506
  row.laserDriftVelocityWest	 =   5.53387; // +/- 1.76426e-05 cm/us All: West = 0.296545 +/- 0.00345096
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53387 +/- 1.76426e-05
  return (TDataSet *)tableSet;// West = 5.53287 +/- 1.93852e-05 East = 5.5387 +/- 4.25765e-05
};
