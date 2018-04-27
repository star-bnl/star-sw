TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 107002
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54583; // +/- 1.11847e-05 cm/us All: East = -0.118802 +/- 0.0025719
  row.laserDriftVelocityWest	 =   5.54583; // +/- 1.11847e-05 cm/us All: West = 0.190159 +/- 0.00321265
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54583 +/- 1.11847e-05
  return (TDataSet *)tableSet;// West = 5.54482 +/- 1.78855e-05 East = 5.54648 +/- 1.4333e-05
};
