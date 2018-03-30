TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 73033
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5537; // +/- 1.00333e-05 cm/us All: East = -2.13452 +/- 0.00637833
  row.laserDriftVelocityWest	 =   5.5537; // +/- 1.00333e-05 cm/us All: West = -2.10824 +/- 0.00184981
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5537 +/- 1.00333e-05
  return (TDataSet *)tableSet;// West = 5.55369 +/- 1.04281e-05 East = 5.55389 +/- 3.6811e-05
};
