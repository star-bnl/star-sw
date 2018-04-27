TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 109047
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54364; // +/- 5.38538e-06 cm/us All: East = -0.617943 +/- 0.00514405
  row.laserDriftVelocityWest	 =   5.54364; // +/- 5.38538e-06 cm/us All: West = 0.187428 +/- 0.000966399
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54364 +/- 5.38538e-06
  return (TDataSet *)tableSet;// West = 5.5435 +/- 5.4872e-06 East = 5.54752 +/- 2.80859e-05
};
