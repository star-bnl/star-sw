TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 54028
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.50913; // +/- 7.21002e-05 cm/us All: East = 0.112972 +/- 0.0288716
  row.laserDriftVelocityWest	 =   5.50913; // +/- 7.21002e-05 cm/us All: West = 0.304998 +/- 0.0806651
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.50913 +/- 7.21002e-05
  return (TDataSet *)tableSet;// West = 5.50868 +/- 0.000109419 East = 5.50948 +/- 9.58526e-05
};
