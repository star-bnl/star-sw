TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 109004
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54376; // +/- 9.00781e-06 cm/us All: East = -0.435504 +/- 0.0047519
  row.laserDriftVelocityWest	 =   5.54376; // +/- 9.00781e-06 cm/us All: West = 0.263249 +/- 0.00172102
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54376 +/- 9.00781e-06
  return (TDataSet *)tableSet;// West = 5.54337 +/- 9.54038e-06 East = 5.54696 +/- 2.73428e-05
};
