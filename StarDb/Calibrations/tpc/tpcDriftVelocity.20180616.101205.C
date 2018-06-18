TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 167012
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54703; // +/- 5.75589e-06 cm/us All: East = -0.22635 +/- 0.00434311
  row.laserDriftVelocityWest	 =   5.54703; // +/- 5.75589e-06 cm/us All: West = 0.156535 +/- 0.00105044
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54703 +/- 5.75589e-06
  return (TDataSet *)tableSet;// West = 5.54691 +/- 5.92903e-06 East = 5.54901 +/- 2.39929e-05
};
