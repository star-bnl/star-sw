TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 55042
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5042; // +/- 1.62792e-05 cm/us All: East = -0.207215 +/- 0.00632138
  row.laserDriftVelocityWest	 =   5.5042; // +/- 1.62792e-05 cm/us All: West = 0.263924 +/- 0.00357969
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5042 +/- 1.62792e-05
  return (TDataSet *)tableSet;// West = 5.50368 +/- 1.8958e-05 East = 5.50568 +/- 3.17656e-05
};
