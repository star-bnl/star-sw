TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 139072
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55905; // +/- 5.44786e-06 cm/us All: East = -0.150407 +/- 0.00241131
  row.laserDriftVelocityWest	 =   5.55905; // +/- 5.44786e-06 cm/us All: West = 0.228829 +/- 0.00104977
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55905 +/- 5.44786e-06
  return (TDataSet *)tableSet;// West = 5.55871 +/- 5.94243e-06 East = 5.56081 +/- 1.36398e-05
};
