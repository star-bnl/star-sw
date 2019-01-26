TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 25001
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55818; // +/- 1.03479e-05 cm/us All: East = -0.48428 +/- 0.0034963
  row.laserDriftVelocityWest	 =   5.55818; // +/- 1.03479e-05 cm/us All: West = 0.10723 +/- 0.00216243
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55818 +/- 1.03479e-05
  return (TDataSet *)tableSet;// West = 5.5573 +/- 1.20895e-05 East = 5.56057 +/- 2.00127e-05
};
