TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 124048
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53333; // +/- 8.52836e-06 cm/us All: East = -1.13933 +/- 0.00246192
  row.laserDriftVelocityWest	 =   5.53333; // +/- 8.52836e-06 cm/us All: West = -0.659479 +/- 0.00194529
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53333 +/- 8.52836e-06
  return (TDataSet *)tableSet;// West = 5.53237 +/- 1.08127e-05 East = 5.53491 +/- 1.38733e-05
};
