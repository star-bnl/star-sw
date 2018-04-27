TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 83055
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52413; // +/- 9.72833e-06 cm/us All: East = 1.47357 +/- 0.0063233
  row.laserDriftVelocityWest	 =   5.52413; // +/- 9.72833e-06 cm/us All: West = 2.02728 +/- 0.00182583
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52413 +/- 9.72833e-06
  return (TDataSet *)tableSet;// West = 5.52391 +/- 1.0108e-05 East = 5.52691 +/- 3.58306e-05
};
