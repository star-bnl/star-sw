TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 120059
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53408; // +/- 6.94712e-06 cm/us All: East = -0.015833 +/- 0.00280979
  row.laserDriftVelocityWest	 =   5.53408; // +/- 6.94712e-06 cm/us All: West = 0.258072 +/- 0.00140253
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53408 +/- 6.94712e-06
  return (TDataSet *)tableSet;// West = 5.53372 +/- 7.85701e-06 East = 5.53534 +/- 1.48722e-05
};
