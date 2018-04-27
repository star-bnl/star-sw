TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 83010
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52423; // +/- 9.96226e-06 cm/us All: East = -0.659923 +/- 15.9097
  row.laserDriftVelocityWest	 =   5.52423; // +/- 9.96226e-06 cm/us All: West = 2.0212 +/- 0.00178104
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52423 +/- 9.96226e-06
  return (TDataSet *)tableSet;// West = 5.52423 +/- 9.96389e-06 East = 5.52624 +/- 0.000551304
};
