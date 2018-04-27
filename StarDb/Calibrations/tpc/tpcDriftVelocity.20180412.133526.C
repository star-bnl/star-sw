TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 102034
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54337; // +/- 1.69079e-05 cm/us All: East = 0.0350269 +/- 0.00482007
  row.laserDriftVelocityWest	 =   5.54337; // +/- 1.69079e-05 cm/us All: West = 0.210769 +/- 0.00430927
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54337 +/- 1.69079e-05
  return (TDataSet *)tableSet;// West = 5.54291 +/- 2.30938e-05 East = 5.54389 +/- 2.48225e-05
};
