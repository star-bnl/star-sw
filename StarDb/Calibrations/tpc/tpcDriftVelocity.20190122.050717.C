TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 22001
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5634; // +/- 9.35958e-06 cm/us All: East = -0.765047 +/- 0.00354042
  row.laserDriftVelocityWest	 =   5.5634; // +/- 9.35958e-06 cm/us All: West = -0.356421 +/- 0.00190636
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5634 +/- 9.35958e-06
  return (TDataSet *)tableSet;// West = 5.56296 +/- 1.06003e-05 East = 5.56495 +/- 1.99371e-05
};
