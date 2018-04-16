TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 105019
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55299; // +/- 8.16172e-06 cm/us All: East = -5.5504 +/- 0.00198553
  row.laserDriftVelocityWest	 =   5.55299; // +/- 8.16172e-06 cm/us All: West = -5.3637 +/- 0.0021664
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55299 +/- 8.16172e-06
  return (TDataSet *)tableSet;// West = 5.55241 +/- 1.21076e-05 East = 5.55347 +/- 1.10496e-05
};
