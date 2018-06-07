TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 149048
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5501; // +/- 5.9892e-06 cm/us All: East = -0.282939 +/- 0.00509341
  row.laserDriftVelocityWest	 =   5.5501; // +/- 5.9892e-06 cm/us All: West = 0.191526 +/- 0.00107364
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5501 +/- 5.9892e-06
  return (TDataSet *)tableSet;// West = 5.55002 +/- 6.10284e-06 East = 5.55231 +/- 3.11815e-05
};
