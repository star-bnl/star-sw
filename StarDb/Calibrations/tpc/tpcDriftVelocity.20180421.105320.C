TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 111016
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5537; // +/- 5.87998e-06 cm/us All: East = 0.0251628 +/- 0.00206136
  row.laserDriftVelocityWest	 =   5.5537; // +/- 5.87998e-06 cm/us All: West = 0.225042 +/- 0.00122323
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5537 +/- 5.87998e-06
  return (TDataSet *)tableSet;// West = 5.5534 +/- 6.86149e-06 East = 5.55452 +/- 1.14087e-05
};
