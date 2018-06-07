TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 147047
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55024; // +/- 4.39062e-06 cm/us All: East = 0.0705803 +/- 0.00338536
  row.laserDriftVelocityWest	 =   5.55024; // +/- 4.39062e-06 cm/us All: West = 0.160109 +/- 0.000793697
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55024 +/- 4.39062e-06
  return (TDataSet *)tableSet;// West = 5.55021 +/- 4.51449e-06 East = 5.55073 +/- 1.88725e-05
};
