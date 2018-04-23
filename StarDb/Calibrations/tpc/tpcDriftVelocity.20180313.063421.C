TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 72010
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54225; // +/- 1.48022e-05 cm/us All: East = 0.0976241 +/- 0.00925049
  row.laserDriftVelocityWest	 =   5.54225; // +/- 1.48022e-05 cm/us All: West = 0.23332 +/- 0.00276456
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54225 +/- 1.48022e-05
  return (TDataSet *)tableSet;// West = 5.54219 +/- 1.54555e-05 East = 5.543 +/- 5.14549e-05
};
