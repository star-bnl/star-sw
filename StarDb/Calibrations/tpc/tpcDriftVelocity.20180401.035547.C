TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 90055
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52831; // +/- 1.15995e-05 cm/us All: East = -3.03911 +/- 0.00920301
  row.laserDriftVelocityWest	 =   5.52831; // +/- 1.15995e-05 cm/us All: West = -2.12719 +/- 0.00211974
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52831 +/- 1.15995e-05
  return (TDataSet *)tableSet;// West = 5.52811 +/- 1.18517e-05 East = 5.53306 +/- 5.65256e-05
};
