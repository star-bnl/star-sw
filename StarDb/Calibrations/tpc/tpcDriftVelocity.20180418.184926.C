TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 108038
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54445; // +/- 8.47097e-06 cm/us All: East = 1.87795 +/- 0.00314413
  row.laserDriftVelocityWest	 =   5.54445; // +/- 8.47097e-06 cm/us All: West = 2.24594 +/- 0.00174733
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54445 +/- 8.47097e-06
  return (TDataSet *)tableSet;// West = 5.54396 +/- 9.79261e-06 East = 5.5459 +/- 1.68843e-05
};
