TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 78018
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5533; // +/- 1.13602e-05 cm/us All: East = 0.383509 +/- 0.0165952
  row.laserDriftVelocityWest	 =   5.5533; // +/- 1.13602e-05 cm/us All: West = 1.17679 +/- 0.00206164
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5533 +/- 1.13602e-05
  return (TDataSet *)tableSet;// West = 5.5532 +/- 1.14842e-05 East = 5.55768 +/- 7.75129e-05
};
