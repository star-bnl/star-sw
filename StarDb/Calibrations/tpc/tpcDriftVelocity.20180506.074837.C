TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 126009
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55891; // +/- 6.73228e-06 cm/us All: East = -0.278677 +/- 0.00190472
  row.laserDriftVelocityWest	 =   5.55891; // +/- 6.73228e-06 cm/us All: West = -0.164492 +/- 0.0015512
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55891 +/- 6.73228e-06
  return (TDataSet *)tableSet;// West = 5.55867 +/- 8.66789e-06 East = 5.55926 +/- 1.06882e-05
};
