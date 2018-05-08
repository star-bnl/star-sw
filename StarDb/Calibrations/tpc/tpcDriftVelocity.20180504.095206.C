TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 124014
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52643; // +/- 1.2173e-05 cm/us All: East = -1.15252 +/- 0.0029083
  row.laserDriftVelocityWest	 =   5.52643; // +/- 1.2173e-05 cm/us All: West = -0.353473 +/- 0.00340038
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52643 +/- 1.2173e-05
  return (TDataSet *)tableSet;// West = 5.52392 +/- 1.86552e-05 East = 5.52829 +/- 1.60643e-05
};
