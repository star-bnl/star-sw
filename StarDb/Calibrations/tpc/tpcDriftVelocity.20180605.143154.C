TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 156031
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55414; // +/- 9.72117e-06 cm/us All: East = -0.0483908 +/- 0.00463606
  row.laserDriftVelocityWest	 =   5.55414; // +/- 9.72117e-06 cm/us All: West = 0.381864 +/- 0.00182894
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55414 +/- 9.72117e-06
  return (TDataSet *)tableSet;// West = 5.55389 +/- 1.03002e-05 East = 5.55614 +/- 2.94083e-05
};
