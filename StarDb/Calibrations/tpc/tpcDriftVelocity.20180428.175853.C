TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 118033
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53832; // +/- 1.15659e-05 cm/us All: East = -0.659352 +/- 0.00618302
  row.laserDriftVelocityWest	 =   5.53832; // +/- 1.15659e-05 cm/us All: West = 0.164853 +/- 0.0021874
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53832 +/- 1.15659e-05
  return (TDataSet *)tableSet;// West = 5.53781 +/- 1.22626e-05 East = 5.54243 +/- 3.48085e-05
};
