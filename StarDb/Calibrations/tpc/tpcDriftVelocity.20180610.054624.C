TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 161004
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54918; // +/- 6.53135e-06 cm/us All: East = -0.410007 +/- 0.00218758
  row.laserDriftVelocityWest	 =   5.54918; // +/- 6.53135e-06 cm/us All: West = 0.262647 +/- 0.00136237
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54918 +/- 6.53135e-06
  return (TDataSet *)tableSet;// West = 5.54816 +/- 7.679e-06 East = 5.55187 +/- 1.24194e-05
};
