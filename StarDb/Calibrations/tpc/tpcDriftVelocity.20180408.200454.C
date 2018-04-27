TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 98038
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54024; // +/- 1.01289e-05 cm/us All: East = 0.0247548 +/- 0.00240571
  row.laserDriftVelocityWest	 =   5.54024; // +/- 1.01289e-05 cm/us All: West = 0.356595 +/- 0.00275258
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54024 +/- 1.01289e-05
  return (TDataSet *)tableSet;// West = 5.53923 +/- 1.51772e-05 East = 5.54105 +/- 1.3601e-05
};
