TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 22004
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.56365; // +/- 9.02753e-06 cm/us All: East = -0.999247 +/- 0.00369566
  row.laserDriftVelocityWest	 =   5.56365; // +/- 9.02753e-06 cm/us All: West = -0.353021 +/- 0.00177867
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.56365 +/- 9.02753e-06
  return (TDataSet *)tableSet;// West = 5.56295 +/- 1.00596e-05 East = 5.56658 +/- 2.0461e-05
};
