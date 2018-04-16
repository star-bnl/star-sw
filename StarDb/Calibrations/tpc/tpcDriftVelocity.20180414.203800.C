TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 104036
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55337; // +/- 7.0949e-06 cm/us All: East = -5.68531 +/- 0.00177554
  row.laserDriftVelocityWest	 =   5.55337; // +/- 7.0949e-06 cm/us All: West = -5.38649 +/- 0.00180011
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55337 +/- 7.0949e-06
  return (TDataSet *)tableSet;// West = 5.55253 +/- 1.00855e-05 East = 5.5542 +/- 9.9827e-06
};
