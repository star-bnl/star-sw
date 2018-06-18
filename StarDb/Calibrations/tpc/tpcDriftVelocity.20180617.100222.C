TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 168014
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54622; // +/- 5.88049e-06 cm/us All: East = -0.219744 +/- 0.00300912
  row.laserDriftVelocityWest	 =   5.54622; // +/- 5.88049e-06 cm/us All: West = -0.0722539 +/- 0.00111222
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54622 +/- 5.88049e-06
  return (TDataSet *)tableSet;// West = 5.54613 +/- 6.27335e-06 East = 5.54689 +/- 1.68825e-05
};
