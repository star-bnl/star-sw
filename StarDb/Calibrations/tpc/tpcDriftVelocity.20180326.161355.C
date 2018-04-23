TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 85025
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5289; // +/- 1.74793e-05 cm/us All: East = -0.127248 +/- 0.00614429
  row.laserDriftVelocityWest	 =   5.5289; // +/- 1.74793e-05 cm/us All: West = 0.246067 +/- 0.00384207
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5289 +/- 1.74793e-05
  return (TDataSet *)tableSet;// West = 5.52849 +/- 2.03863e-05 East = 5.53003 +/- 3.3964e-05
};
