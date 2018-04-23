TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 94043
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52039; // +/- 8.88831e-06 cm/us All: East = -0.0507178 +/- 0.00333546
  row.laserDriftVelocityWest	 =   5.52039; // +/- 8.88831e-06 cm/us All: West = 0.239814 +/- 0.00182379
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52039 +/- 8.88831e-06
  return (TDataSet *)tableSet;// West = 5.52004 +/- 1.01023e-05 East = 5.52159 +/- 1.87006e-05
};
