TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 136046
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53849; // +/- 4.91639e-06 cm/us All: East = 0.22318 +/- 0.00187092
  row.laserDriftVelocityWest	 =   5.53849; // +/- 4.91639e-06 cm/us All: West = 0.147106 +/- 0.000985549
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53849 +/- 4.91639e-06
  return (TDataSet *)tableSet;// West = 5.53858 +/- 5.58104e-06 East = 5.53819 +/- 1.03878e-05
};
