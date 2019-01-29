TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 29031
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53348; // +/- 2.01304e-05 cm/us All: East = -0.616404 +/- 0.00362098
  row.laserDriftVelocityWest	 =   5.53348; // +/- 2.01304e-05 cm/us All: West = -999 +/- 999
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53348 +/- 2.01304e-05
  return (TDataSet *)tableSet;// West = -999 +/- 999 East = 5.53348 +/- 2.01304e-05
};
