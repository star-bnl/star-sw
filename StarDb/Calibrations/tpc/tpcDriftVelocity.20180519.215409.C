TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 139064
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55912; // +/- 5.27119e-06 cm/us All: East = -0.197407 +/- 0.00208725
  row.laserDriftVelocityWest	 =   5.55912; // +/- 5.27119e-06 cm/us All: West = 0.221089 +/- 0.0010492
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55912 +/- 5.27119e-06
  return (TDataSet *)tableSet;// West = 5.55866 +/- 5.89127e-06 East = 5.56098 +/- 1.18036e-05
};
