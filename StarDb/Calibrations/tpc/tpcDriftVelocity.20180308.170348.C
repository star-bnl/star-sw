TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 67033
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.56523; // +/- 6.67894e-05 cm/us All: East = -0.141714 +/- 0.0928238
  row.laserDriftVelocityWest	 =   5.56523; // +/- 6.67894e-05 cm/us All: West = 0.277792 +/- 0.040369
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.56523 +/- 6.67894e-05
  return (TDataSet *)tableSet;// West = 5.56476 +/- 7.45193e-05 East = 5.56714 +/- 0.000150593
};
