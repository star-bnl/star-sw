TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 107033
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54555; // +/- 1.25853e-05 cm/us All: East = -0.19624 +/- 0.00274823
  row.laserDriftVelocityWest	 =   5.54555; // +/- 1.25853e-05 cm/us All: West = 0.0865507 +/- 0.00411439
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54555 +/- 1.25853e-05
  return (TDataSet *)tableSet;// West = 5.54448 +/- 2.28391e-05 East = 5.54601 +/- 1.50817e-05
};
