TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 85018
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.51998; // +/- 1.2577e-05 cm/us All: East = -0.378198 +/- 0.00782506
  row.laserDriftVelocityWest	 =   5.51998; // +/- 1.2577e-05 cm/us All: West = 0.250676 +/- 0.00233706
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.51998 +/- 1.2577e-05
  return (TDataSet *)tableSet;// West = 5.5197 +/- 1.3127e-05 East = 5.52308 +/- 4.39095e-05
};
