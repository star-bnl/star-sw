TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 90021
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.51734; // +/- 1.10366e-05 cm/us All: East = -0.846624 +/- 0.0122882
  row.laserDriftVelocityWest	 =   5.51734; // +/- 1.10366e-05 cm/us All: West = 0.0417438 +/- 0.00201504
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.51734 +/- 1.10366e-05
  return (TDataSet *)tableSet;// West = 5.51723 +/- 1.1176e-05 East = 5.52189 +/- 7.00916e-05
};
