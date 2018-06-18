TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 165003
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54804; // +/- 0.00023481 cm/us All: East = -0.306548 +/- 0.314071
  row.laserDriftVelocityWest	 =   5.54804; // +/- 0.00023481 cm/us All: West = 0.130343 +/- 0.0685872
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54804 +/- 0.00023481
  return (TDataSet *)tableSet;// West = 5.54712 +/- 0.000261646 East = 5.55185 +/- 0.000532268
};
