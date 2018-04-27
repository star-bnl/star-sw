TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 109013
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54513; // +/- 1.46641e-05 cm/us All: East = -0.705972 +/- 0.00476603
  row.laserDriftVelocityWest	 =   5.54513; // +/- 1.46641e-05 cm/us All: West = 0.221518 +/- 0.00203836
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54513 +/- 1.46641e-05
  return (TDataSet *)tableSet;// West = 5.54347 +/- 1.7863e-05 East = 5.54857 +/- 2.56798e-05
};
