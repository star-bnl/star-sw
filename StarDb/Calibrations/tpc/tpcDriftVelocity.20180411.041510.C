TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 101001
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54371; // +/- 1.01441e-05 cm/us All: East = -0.250184 +/- 0.0018777
  row.laserDriftVelocityWest	 =   5.54371; // +/- 1.01441e-05 cm/us All: West = 0.273037 +/- 0.00884158
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54371 +/- 1.01441e-05
  return (TDataSet *)tableSet;// West = 5.54081 +/- 3.82661e-05 East = 5.54393 +/- 1.05204e-05
};
