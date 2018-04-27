TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 100048
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54171; // +/- 8.1717e-06 cm/us All: East = -0.188454 +/- 0.00229151
  row.laserDriftVelocityWest	 =   5.54171; // +/- 8.1717e-06 cm/us All: West = 0.312218 +/- 0.00190021
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54171 +/- 8.1717e-06
  return (TDataSet *)tableSet;// West = 5.54061 +/- 1.05949e-05 East = 5.54332 +/- 1.28388e-05
};
