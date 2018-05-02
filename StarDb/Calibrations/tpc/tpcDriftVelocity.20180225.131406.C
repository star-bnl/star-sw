TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 56023
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.51028; // +/- 9.71949e-06 cm/us All: East = -0.0493653 +/- 0.00293511
  row.laserDriftVelocityWest	 =   5.51028; // +/- 9.71949e-06 cm/us All: West = 0.358283 +/- 0.00225492
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.51028 +/- 9.71949e-06
  return (TDataSet *)tableSet;// West = 5.50945 +/- 1.22721e-05 East = 5.51167 +/- 1.592e-05
};
