TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 93014
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53005; // +/- 1.51968e-05 cm/us All: East = -1.04031 +/- 0.00742943
  row.laserDriftVelocityWest	 =   5.53005; // +/- 1.51968e-05 cm/us All: West = -0.0634762 +/- 0.00294338
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53005 +/- 1.51968e-05
  return (TDataSet *)tableSet;// West = 5.52938 +/- 1.627e-05 East = 5.5347 +/- 4.25485e-05
};
