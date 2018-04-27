TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 107025
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54545; // +/- 1.33266e-05 cm/us All: East = -0.133479 +/- 0.00315789
  row.laserDriftVelocityWest	 =   5.54545; // +/- 1.33266e-05 cm/us All: West = 0.203443 +/- 0.00377274
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54545 +/- 1.33266e-05
  return (TDataSet *)tableSet;// West = 5.54437 +/- 2.06464e-05 East = 5.54622 +/- 1.74481e-05
};
