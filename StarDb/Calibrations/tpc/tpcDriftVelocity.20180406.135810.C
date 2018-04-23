TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 96026
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54202; // +/- 1.66183e-05 cm/us All: East = -0.331888 +/- 0.00449726
  row.laserDriftVelocityWest	 =   5.54202; // +/- 1.66183e-05 cm/us All: West = 0.515127 +/- 0.00402526
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54202 +/- 1.66183e-05
  return (TDataSet *)tableSet;// West = 5.54007 +/- 2.20541e-05 East = 5.54458 +/- 2.52781e-05
};
