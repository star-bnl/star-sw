TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 110018
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55374; // +/- 7.39672e-06 cm/us All: East = -0.12683 +/- 0.00214169
  row.laserDriftVelocityWest	 =   5.55374; // +/- 7.39672e-06 cm/us All: West = 0.239211 +/- 0.00106845
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55374 +/- 7.39672e-06
  return (TDataSet *)tableSet;// West = 5.55276 +/- 9.43069e-06 East = 5.55531 +/- 1.19235e-05
};
