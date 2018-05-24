TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 137010
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53818; // +/- 5.30824e-06 cm/us All: East = 0.456724 +/- 0.00437416
  row.laserDriftVelocityWest	 =   5.53818; // +/- 5.30824e-06 cm/us All: West = 0.159427 +/- 0.000965379
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53818 +/- 5.30824e-06
  return (TDataSet *)tableSet;// West = 5.53827 +/- 5.45535e-06 East = 5.53654 +/- 2.30131e-05
};
