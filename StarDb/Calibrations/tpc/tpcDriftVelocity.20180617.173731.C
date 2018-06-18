TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 168029
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54606; // +/- 6.85533e-06 cm/us All: East = -0.288964 +/- 0.00416902
  row.laserDriftVelocityWest	 =   5.54606; // +/- 6.85533e-06 cm/us All: West = 0.222223 +/- 0.00128008
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54606 +/- 6.85533e-06
  return (TDataSet *)tableSet;// West = 5.54582 +/- 7.17648e-06 East = 5.54863 +/- 2.31758e-05
};
