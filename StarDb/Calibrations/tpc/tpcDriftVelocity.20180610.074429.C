TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 161008
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54874; // +/- 7.81397e-06 cm/us All: East = -0.465131 +/- 0.003714
  row.laserDriftVelocityWest	 =   5.54874; // +/- 7.81397e-06 cm/us All: West = 0.226689 +/- 0.00148094
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54874 +/- 7.81397e-06
  return (TDataSet *)tableSet;// West = 5.54827 +/- 8.35624e-06 East = 5.552 +/- 2.20505e-05
};
