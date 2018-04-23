TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 103055
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55392; // +/- 8.30113e-06 cm/us All: East = 0.0948968 +/- 0.00200791
  row.laserDriftVelocityWest	 =   5.55392; // +/- 8.30113e-06 cm/us All: West = 0.282628 +/- 0.00216356
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55392 +/- 8.30113e-06
  return (TDataSet *)tableSet;// West = 5.55336 +/- 1.21325e-05 East = 5.55441 +/- 1.13825e-05
};
