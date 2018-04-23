TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 109047
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54318; // +/- 7.60443e-06 cm/us All: East = 1.19305 +/- 0.00501856
  row.laserDriftVelocityWest	 =   5.54318; // +/- 7.60443e-06 cm/us All: West = 2.00752 +/- 0.000977327
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54318 +/- 7.60443e-06
  return (TDataSet *)tableSet;// West = 5.54277 +/- 7.93525e-06 East = 5.5478 +/- 2.66141e-05
};
