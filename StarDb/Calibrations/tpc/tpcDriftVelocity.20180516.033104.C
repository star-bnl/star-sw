TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 135043
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53654; // +/- 7.52576e-06 cm/us All: East = 0.306556 +/- 0.00214655
  row.laserDriftVelocityWest	 =   5.53654; // +/- 7.52576e-06 cm/us All: West = 0.123984 +/- 0.000980504
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53654 +/- 7.52576e-06
  return (TDataSet *)tableSet;// West = 5.53643 +/- 9.74119e-06 East = 5.53669 +/- 1.1853e-05
};
