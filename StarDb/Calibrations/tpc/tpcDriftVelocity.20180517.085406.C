TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 137019
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53822; // +/- 4.73085e-06 cm/us All: East = -0.130354 +/- 0.00247198
  row.laserDriftVelocityWest	 =   5.53822; // +/- 4.73085e-06 cm/us All: West = 0.185083 +/- 0.000892036
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53822 +/- 4.73085e-06
  return (TDataSet *)tableSet;// West = 5.53802 +/- 5.03635e-06 East = 5.53973 +/- 1.37933e-05
};
