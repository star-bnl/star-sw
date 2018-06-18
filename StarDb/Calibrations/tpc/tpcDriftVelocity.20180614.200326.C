TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 165033
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54751; // +/- 6.57178e-06 cm/us All: East = 0.0366458 +/- 0.00283368
  row.laserDriftVelocityWest	 =   5.54751; // +/- 6.57178e-06 cm/us All: West = 0.258904 +/- 0.00127799
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54751 +/- 6.57178e-06
  return (TDataSet *)tableSet;// West = 5.54733 +/- 7.16424e-06 East = 5.54842 +/- 1.6504e-05
};
