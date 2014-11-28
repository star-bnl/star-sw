TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 100098
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52285; // +/- 2.18326e-05 cm/us All: East = -0.361644 +/- 7.05638
  row.laserDriftVelocityWest	 =   5.52285; // +/- 2.18326e-05 cm/us All: West = 0.510485 +/- 0.00347431
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52285 +/- 2.18326e-05
  return (TDataSet *)tableSet;// West = 5.52285 +/- 2.18326e-05 East = 5.52911 +/- 0.108048
};
