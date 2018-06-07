TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 145043
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5522; // +/- 8.82289e-05 cm/us All: East = -0.357141 +/- 0.369417
  row.laserDriftVelocityWest	 =   5.5522; // +/- 8.82289e-05 cm/us All: West = 0.171552 +/- 0.0655118
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5522 +/- 8.82289e-05
  return (TDataSet *)tableSet;// West = 5.55133 +/- 9.76247e-05 East = 5.5561 +/- 0.00020612
};
