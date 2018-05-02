TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 56001
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.50582; // +/- 1.08925e-05 cm/us All: East = -0.181753 +/- 0.00331254
  row.laserDriftVelocityWest	 =   5.50582; // +/- 1.08925e-05 cm/us All: West = 0.360669 +/- 0.00240326
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.50582 +/- 1.08925e-05
  return (TDataSet *)tableSet;// West = 5.50483 +/- 1.34195e-05 East = 5.50772 +/- 1.86487e-05
};
