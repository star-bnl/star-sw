TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 105034
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55353; // +/- 9.07654e-06 cm/us All: East = 0.0495321 +/- 0.00225978
  row.laserDriftVelocityWest	 =   5.55353; // +/- 9.07654e-06 cm/us All: West = 0.322108 +/- 0.00238177
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55353 +/- 9.07654e-06
  return (TDataSet *)tableSet;// West = 5.55276 +/- 1.30994e-05 East = 5.55423 +/- 1.25881e-05
};
