TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 89034
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.51561; // +/- 1.93824e-05 cm/us All: East = -0.815504 +/- 0.0177077
  row.laserDriftVelocityWest	 =   5.51561; // +/- 1.93824e-05 cm/us All: West = 0.20073 +/- 0.00364666
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.51561 +/- 1.93824e-05
  return (TDataSet *)tableSet;// West = 5.51537 +/- 1.9846e-05 East = 5.52074 +/- 9.01954e-05
};
