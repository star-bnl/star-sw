TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 89051
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52552; // +/- 1.05011e-05 cm/us All: East = -2.52027 +/- 0.00957939
  row.laserDriftVelocityWest	 =   5.52552; // +/- 1.05011e-05 cm/us All: West = -1.62461 +/- 0.00189799
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52552 +/- 1.05011e-05
  return (TDataSet *)tableSet;// West = 5.52533 +/- 1.07156e-05 East = 5.53027 +/- 5.27495e-05
};
