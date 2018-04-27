TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 109039
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54406; // +/- 6.19791e-06 cm/us All: East = -0.137757 +/- 0.00208888
  row.laserDriftVelocityWest	 =   5.54406; // +/- 6.19791e-06 cm/us All: West = 0.148597 +/- 0.00129394
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54406 +/- 6.19791e-06
  return (TDataSet *)tableSet;// West = 5.54363 +/- 7.29064e-06 East = 5.54518 +/- 1.17699e-05
};
