TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 93030
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52026; // +/- 9.49457e-06 cm/us All: East = -0.711103 +/- 0.00626375
  row.laserDriftVelocityWest	 =   5.52026; // +/- 9.49457e-06 cm/us All: West = 0.278399 +/- 0.00176892
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52026 +/- 9.49457e-06
  return (TDataSet *)tableSet;// West = 5.51988 +/- 9.8394e-06 East = 5.52532 +/- 3.61813e-05
};
