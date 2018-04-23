TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 85018
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52992; // +/- 0.000130554 cm/us All: East = -0.648538 +/- 3.4152
  row.laserDriftVelocityWest	 =   5.52992; // +/- 0.000130554 cm/us All: West = 0.0917665 +/- 0.246152
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52992 +/- 0.000130554
  return (TDataSet *)tableSet;// West = 5.52992 +/- 0.000130561 East = 5.53539 +/- 0.0121114
};
