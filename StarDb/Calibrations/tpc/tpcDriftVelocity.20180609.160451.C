TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 160024
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54907; // +/- 6.58247e-06 cm/us All: East = 0.181891 +/- 0.00292504
  row.laserDriftVelocityWest	 =   5.54907; // +/- 6.58247e-06 cm/us All: West = 0.201614 +/- 0.00127451
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54907 +/- 6.58247e-06
  return (TDataSet *)tableSet;// West = 5.54904 +/- 7.16418e-06 East = 5.54924 +/- 1.66765e-05
};
