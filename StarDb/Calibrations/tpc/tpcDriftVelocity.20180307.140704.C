TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 66019
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53168; // +/- 9.20331e-06 cm/us All: East = -0.0893358 +/- 0.0019768
  row.laserDriftVelocityWest	 =   5.53168; // +/- 9.20331e-06 cm/us All: West = 0.451525 +/- 0.00289582
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53168 +/- 9.20331e-06
  return (TDataSet *)tableSet;// West = 5.52967 +/- 1.63959e-05 East = 5.5326 +/- 1.11205e-05
};
