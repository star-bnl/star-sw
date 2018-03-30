TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 72015
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5431; // +/- 1.11978e-05 cm/us All: East = -999 +/- 999
  row.laserDriftVelocityWest	 =   5.5431; // +/- 1.11978e-05 cm/us All: West = -0.200357 +/- 0.00198564
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5431 +/- 1.11978e-05
  return (TDataSet *)tableSet;// West = 5.5431 +/- 1.11978e-05 East = -999 +/- 999
};
