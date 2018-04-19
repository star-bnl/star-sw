TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 107033
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5555; // +/- 1.3504e-05 cm/us All: East = 0.11351 +/- 0.0029713
  row.laserDriftVelocityWest	 =   5.5555; // +/- 1.3504e-05 cm/us All: West = 0.396654 +/- 0.00430557
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5555 +/- 1.3504e-05
  return (TDataSet *)tableSet;// West = 5.55442 +/- 2.41528e-05 East = 5.55599 +/- 1.62876e-05
};
