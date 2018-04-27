TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 111008
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54368; // +/- 5.78607e-06 cm/us All: East = 0.0216104 +/- 0.00237623
  row.laserDriftVelocityWest	 =   5.54368; // +/- 5.78607e-06 cm/us All: West = 0.19509 +/- 0.00113285
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54368 +/- 5.78607e-06
  return (TDataSet *)tableSet;// West = 5.5435 +/- 6.43656e-06 East = 5.54446 +/- 1.3208e-05
};
