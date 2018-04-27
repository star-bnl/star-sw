TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 99010
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54096; // +/- 1.61041e-05 cm/us All: East = -0.112318 +/- 0.00588152
  row.laserDriftVelocityWest	 =   5.54096; // +/- 1.61041e-05 cm/us All: West = 0.708957 +/- 0.00334987
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54096 +/- 1.61041e-05
  return (TDataSet *)tableSet;// West = 5.53989 +/- 1.83764e-05 East = 5.54447 +/- 3.3433e-05
};
