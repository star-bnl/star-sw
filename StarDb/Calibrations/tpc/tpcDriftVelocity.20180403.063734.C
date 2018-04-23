TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 93005
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52969; // +/- 9.09659e-06 cm/us All: East = -0.469643 +/- 0.00424486
  row.laserDriftVelocityWest	 =   5.52969; // +/- 9.09659e-06 cm/us All: West = 0.294746 +/- 0.00175313
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52969 +/- 9.09659e-06
  return (TDataSet *)tableSet;// West = 5.5291 +/- 9.80829e-06 East = 5.53333 +/- 2.4324e-05
};
