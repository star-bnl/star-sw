TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 121007
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53348; // +/- 6.31735e-06 cm/us All: East = -0.180931 +/- 0.00219441
  row.laserDriftVelocityWest	 =   5.53348; // +/- 6.31735e-06 cm/us All: West = 0.22612 +/- 0.00131494
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53348 +/- 6.31735e-06
  return (TDataSet *)tableSet;// West = 5.5329 +/- 7.37044e-06 East = 5.5351 +/- 1.22639e-05
};
