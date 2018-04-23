TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 80083
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55426; // +/- 1.10827e-05 cm/us All: East = -0.137099 +/- 0.00712106
  row.laserDriftVelocityWest	 =   5.55426; // +/- 1.10827e-05 cm/us All: West = 0.201392 +/- 0.00207375
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55426 +/- 1.10827e-05
  return (TDataSet *)tableSet;// West = 5.55412 +/- 1.15187e-05 East = 5.556 +/- 4.06655e-05
};
