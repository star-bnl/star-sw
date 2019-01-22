TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 21069
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.56322; // +/- 9.22031e-06 cm/us All: East = -0.773974 +/- 0.00300759
  row.laserDriftVelocityWest	 =   5.56322; // +/- 9.22031e-06 cm/us All: West = -0.262103 +/- 0.00195986
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.56322 +/- 9.22031e-06
  return (TDataSet *)tableSet;// West = 5.56245 +/- 1.08626e-05 East = 5.56521 +/- 1.74399e-05
};
