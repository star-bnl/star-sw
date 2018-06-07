TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 148036
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55079; // +/- 0.000167848 cm/us All: East = 0.143544 +/- 0.0952316
  row.laserDriftVelocityWest	 =   5.55079; // +/- 0.000167848 cm/us All: West = -0.0120122 +/- 0.048293
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55079 +/- 0.000167848
  return (TDataSet *)tableSet;// West = 5.54465 +/- 0.00848797 East = 5.55079 +/- 0.00016788
};
