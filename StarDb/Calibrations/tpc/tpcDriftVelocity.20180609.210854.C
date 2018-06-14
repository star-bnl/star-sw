TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 160033
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54908; // +/- 6.08831e-06 cm/us All: East = 0.115617 +/- 0.0025502
  row.laserDriftVelocityWest	 =   5.54908; // +/- 6.08831e-06 cm/us All: West = 0.162271 +/- 0.00119809
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54908 +/- 6.08831e-06
  return (TDataSet *)tableSet;// West = 5.54904 +/- 6.74121e-06 East = 5.54928 +/- 1.4181e-05
};
