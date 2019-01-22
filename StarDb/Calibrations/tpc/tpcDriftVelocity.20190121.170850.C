TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 21046
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.56195; // +/- 8.73442e-06 cm/us All: East = -0.683212 +/- 0.00374542
  row.laserDriftVelocityWest	 =   5.56195; // +/- 8.73442e-06 cm/us All: West = -0.0770941 +/- 0.00171066
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.56195 +/- 8.73442e-06
  return (TDataSet *)tableSet;// West = 5.56139 +/- 9.57825e-06 East = 5.5647 +/- 2.12821e-05
};
