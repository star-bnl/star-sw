TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 114017
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54072; // +/- 1.08106e-05 cm/us All: East = -0.642722 +/- 0.00648214
  row.laserDriftVelocityWest	 =   5.54072; // +/- 1.08106e-05 cm/us All: West = 0.1526 +/- 0.00202136
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54072 +/- 1.08106e-05
  return (TDataSet *)tableSet;// West = 5.54035 +/- 1.13265e-05 East = 5.54452 +/- 3.62326e-05
};
