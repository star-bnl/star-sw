TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 96045
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53435; // +/- 1.40133e-05 cm/us All: East = 0.312171 +/- 0.00344518
  row.laserDriftVelocityWest	 =   5.53435; // +/- 1.40133e-05 cm/us All: West = 0.851635 +/- 0.00374649
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53435 +/- 1.40133e-05
  return (TDataSet *)tableSet;// West = 5.53277 +/- 2.05729e-05 East = 5.53572 +/- 1.91402e-05
};
