TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 120051
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53379; // +/- 6.19235e-06 cm/us All: East = 0.0410324 +/- 0.00171407
  row.laserDriftVelocityWest	 =   5.53379; // +/- 6.19235e-06 cm/us All: West = 0.24844 +/- 0.00144523
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53379 +/- 6.19235e-06
  return (TDataSet *)tableSet;// West = 5.53333 +/- 8.09651e-06 East = 5.53445 +/- 9.61176e-06
};
