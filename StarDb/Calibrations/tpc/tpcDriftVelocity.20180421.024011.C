TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 110049
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55354; // +/- 6.09448e-06 cm/us All: East = -0.51181 +/- 0.0047328
  row.laserDriftVelocityWest	 =   5.55354; // +/- 6.09448e-06 cm/us All: West = 0.23926 +/- 0.001115
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55354 +/- 6.09448e-06
  return (TDataSet *)tableSet;// West = 5.55335 +/- 6.25172e-06 East = 5.55734 +/- 2.73452e-05
};
