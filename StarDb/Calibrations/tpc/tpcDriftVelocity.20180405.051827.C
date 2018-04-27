TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 95004
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52131; // +/- 1.11883e-05 cm/us All: East = -0.556116 +/- 0.00531477
  row.laserDriftVelocityWest	 =   5.52131; // +/- 1.11883e-05 cm/us All: West = 0.186583 +/- 0.00218296
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52131 +/- 1.11883e-05
  return (TDataSet *)tableSet;// West = 5.52072 +/- 1.21911e-05 East = 5.5245 +/- 2.81702e-05
};
