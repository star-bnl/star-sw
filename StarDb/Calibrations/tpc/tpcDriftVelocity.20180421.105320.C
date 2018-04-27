TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 111016
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54368; // +/- 5.74162e-06 cm/us All: East = 0.00954332 +/- 0.00197785
  row.laserDriftVelocityWest	 =   5.54368; // +/- 5.74162e-06 cm/us All: West = 0.214814 +/- 0.00117959
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54368 +/- 5.74162e-06
  return (TDataSet *)tableSet;// West = 5.54338 +/- 6.70597e-06 East = 5.5445 +/- 1.11131e-05
};
