TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 111008
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5537; // +/- 5.9167e-06 cm/us All: East = 0.00386781 +/- 0.00246103
  row.laserDriftVelocityWest	 =   5.5537; // +/- 5.9167e-06 cm/us All: West = 0.210073 +/- 0.0011492
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5537 +/- 5.9167e-06
  return (TDataSet *)tableSet;// West = 5.55349 +/- 6.55773e-06 East = 5.55462 +/- 1.37209e-05
};
