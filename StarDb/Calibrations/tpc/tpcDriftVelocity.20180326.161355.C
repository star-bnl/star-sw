TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 85025
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.51891; // +/- 1.67651e-05 cm/us All: East = -0.136234 +/- 0.0054436
  row.laserDriftVelocityWest	 =   5.51891; // +/- 1.67651e-05 cm/us All: West = 0.264269 +/- 0.00362502
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.51891 +/- 1.67651e-05
  return (TDataSet *)tableSet;// West = 5.5184 +/- 1.98617e-05 East = 5.52017 +/- 3.12661e-05
};
