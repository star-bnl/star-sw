TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 149018
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54994; // +/- 8.51561e-06 cm/us All: East = -0.00528834 +/- 0.00355219
  row.laserDriftVelocityWest	 =   5.54994; // +/- 8.51561e-06 cm/us All: West = 0.136614 +/- 0.000986887
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54994 +/- 8.51561e-06
  return (TDataSet *)tableSet;// West = 5.54965 +/- 9.44474e-06 East = 5.55123 +/- 1.96884e-05
};
