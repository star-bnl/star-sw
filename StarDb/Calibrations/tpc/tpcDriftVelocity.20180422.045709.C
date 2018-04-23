TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 112003
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55348; // +/- 7.69471e-06 cm/us All: East = -0.0922061 +/- 0.00353835
  row.laserDriftVelocityWest	 =   5.55348; // +/- 7.69471e-06 cm/us All: West = 0.343351 +/- 0.00151561
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55348 +/- 7.69471e-06
  return (TDataSet *)tableSet;// West = 5.55316 +/- 8.33671e-06 East = 5.55528 +/- 1.99955e-05
};
