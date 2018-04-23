TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 106023
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55583; // +/- 1.02929e-05 cm/us All: East = 0.0688287 +/- 0.00245472
  row.laserDriftVelocityWest	 =   5.55583; // +/- 1.02929e-05 cm/us All: West = 0.296551 +/- 0.00280423
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55583 +/- 1.02929e-05
  return (TDataSet *)tableSet;// West = 5.5551 +/- 1.5443e-05 East = 5.55641 +/- 1.38067e-05
};
