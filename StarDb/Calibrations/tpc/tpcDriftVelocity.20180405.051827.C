TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 95004
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53122; // +/- 1.14509e-05 cm/us All: East = -0.559942 +/- 0.00573809
  row.laserDriftVelocityWest	 =   5.53122; // +/- 1.14509e-05 cm/us All: West = 0.237178 +/- 0.00222331
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53122 +/- 1.14509e-05
  return (TDataSet *)tableSet;// West = 5.53064 +/- 1.23951e-05 East = 5.5346 +/- 2.99127e-05
};
