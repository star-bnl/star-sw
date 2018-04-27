TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 98047
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54043; // +/- 1.10316e-05 cm/us All: East = 0.0569773 +/- 0.00260012
  row.laserDriftVelocityWest	 =   5.54043; // +/- 1.10316e-05 cm/us All: West = 0.363845 +/- 0.00308302
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54043 +/- 1.10316e-05
  return (TDataSet *)tableSet;// West = 5.53947 +/- 1.71299e-05 East = 5.54111 +/- 1.44199e-05
};
