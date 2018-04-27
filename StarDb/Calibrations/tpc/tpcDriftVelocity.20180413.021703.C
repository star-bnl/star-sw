TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 102052
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54383; // +/- 1.81369e-05 cm/us All: East = -0.490415 +/- 0.658026
  row.laserDriftVelocityWest	 =   5.54383; // +/- 1.81369e-05 cm/us All: West = 0.178287 +/- 0.00327511
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54383 +/- 1.81369e-05
  return (TDataSet *)tableSet;// West = 5.54383 +/- 1.81404e-05 East = 5.54593 +/- 0.000927541
};
