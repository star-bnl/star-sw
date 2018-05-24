TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 139043
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55793; // +/- 4.94835e-06 cm/us All: East = -0.210441 +/- 0.00215811
  row.laserDriftVelocityWest	 =   5.55793; // +/- 4.94835e-06 cm/us All: West = 0.200481 +/- 0.000957435
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55793 +/- 4.94835e-06
  return (TDataSet *)tableSet;// West = 5.55756 +/- 5.41859e-06 East = 5.5598 +/- 1.2144e-05
};
