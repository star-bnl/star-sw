TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 66016
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53098; // +/- 1.07951e-05 cm/us All: East = -0.245479 +/- 0.00330951
  row.laserDriftVelocityWest	 =   5.53098; // +/- 1.07951e-05 cm/us All: West = 0.469254 +/- 0.00235002
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53098 +/- 1.07951e-05
  return (TDataSet *)tableSet;// West = 5.52962 +/- 1.31914e-05 East = 5.53374 +/- 1.87829e-05
};
