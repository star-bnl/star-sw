TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 80083
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54427; // +/- 1.10636e-05 cm/us All: East = 1.65239 +/- 0.00796669
  row.laserDriftVelocityWest	 =   5.54427; // +/- 1.10636e-05 cm/us All: West = 2.05424 +/- 0.00204339
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54427 +/- 1.10636e-05
  return (TDataSet *)tableSet;// West = 5.54413 +/- 1.14292e-05 East = 5.54638 +/- 4.4096e-05
};
