TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 83018
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53411; // +/- 9.98739e-06 cm/us All: East = -0.245804 +/- 0.0170839
  row.laserDriftVelocityWest	 =   5.53411; // +/- 9.98739e-06 cm/us All: West = 0.190159 +/- 0.00180323
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53411 +/- 9.98739e-06
  return (TDataSet *)tableSet;// West = 5.53407 +/- 1.00618e-05 East = 5.53659 +/- 8.22599e-05
};
