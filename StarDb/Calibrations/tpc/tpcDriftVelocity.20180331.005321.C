TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 89060
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.51628; // +/- 9.86335e-06 cm/us All: East = -0.500416 +/- 0.00789587
  row.laserDriftVelocityWest	 =   5.51628; // +/- 9.86335e-06 cm/us All: West = 0.230801 +/- 0.00181539
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.51628 +/- 9.86335e-06
  return (TDataSet *)tableSet;// West = 5.51607 +/- 1.01265e-05 East = 5.52007 +/- 4.35516e-05
};
