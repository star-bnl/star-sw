TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 90047
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.51806; // +/- 4.50248e-05 cm/us All: East = -0.440639 +/- 0.235468
  row.laserDriftVelocityWest	 =   5.51806; // +/- 4.50248e-05 cm/us All: West = 0.21273 +/- 0.0219381
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.51806 +/- 4.50248e-05
  return (TDataSet *)tableSet;// West = 5.51779 +/- 4.66402e-05 East = 5.52189 +/- 0.000172571
};
