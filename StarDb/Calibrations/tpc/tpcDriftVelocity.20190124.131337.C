TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 24017
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55847; // +/- 1.17324e-05 cm/us All: East = -0.00859928 +/- 0.00909548
  row.laserDriftVelocityWest	 =   5.55847; // +/- 1.17324e-05 cm/us All: West = 0.868457 +/- 0.00215104
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55847 +/- 1.17324e-05
  return (TDataSet *)tableSet;// West = 5.55825 +/- 1.20951e-05 East = 5.56187 +/- 4.82704e-05
};
