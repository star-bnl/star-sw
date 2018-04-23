TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 105009
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5525; // +/- 1.18995e-05 cm/us All: East = -0.640326 +/- 0.814577
  row.laserDriftVelocityWest	 =   5.5525; // +/- 1.18995e-05 cm/us All: West = 0.179816 +/- 0.00214277
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5525 +/- 1.18995e-05
  return (TDataSet *)tableSet;// West = 5.5525 +/- 1.19003e-05 East = 5.55635 +/- 0.000998397
};
