TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 121038
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53189; // +/- 7.99783e-06 cm/us All: East = -0.62347 +/- 0.00489448
  row.laserDriftVelocityWest	 =   5.53189; // +/- 7.99783e-06 cm/us All: West = 0.296338 +/- 0.00150327
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53189 +/- 7.99783e-06
  return (TDataSet *)tableSet;// West = 5.53146 +/- 8.38167e-06 East = 5.5362 +/- 2.6735e-05
};
