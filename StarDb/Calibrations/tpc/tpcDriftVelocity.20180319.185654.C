TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 78023
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55319; // +/- 1.18764e-05 cm/us All: East = -2.08035 +/- 0.00407211
  row.laserDriftVelocityWest	 =   5.55319; // +/- 1.18764e-05 cm/us All: West = -1.99294 +/- 0.00255598
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55319 +/- 1.18764e-05
  return (TDataSet *)tableSet;// West = 5.55303 +/- 1.39852e-05 East = 5.55358 +/- 2.24913e-05
};
