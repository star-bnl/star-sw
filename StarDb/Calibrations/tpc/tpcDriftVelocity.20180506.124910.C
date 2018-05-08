TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 126025
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.56145; // +/- 6.46196e-06 cm/us All: East = -0.749133 +/- 0.00168519
  row.laserDriftVelocityWest	 =   5.56145; // +/- 6.46196e-06 cm/us All: West = -0.593657 +/- 0.0015492
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.56145 +/- 6.46196e-06
  return (TDataSet *)tableSet;// West = 5.56106 +/- 8.73063e-06 East = 5.56191 +/- 9.60967e-06
};
