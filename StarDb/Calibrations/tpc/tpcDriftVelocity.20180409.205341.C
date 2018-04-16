TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 99035
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55313; // +/- 1.84309e-05 cm/us All: East = -5.65773 +/- 0.00366338
  row.laserDriftVelocityWest	 =   5.55313; // +/- 1.84309e-05 cm/us All: West = -4.91435 +/- 0.00787614
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55313 +/- 1.84309e-05
  return (TDataSet *)tableSet;// West = 5.54989 +/- 4.05242e-05 East = 5.55398 +/- 2.06951e-05
};
