TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 137002
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5383; // +/- 5.22288e-06 cm/us All: East = 0.31571 +/- 0.00199333
  row.laserDriftVelocityWest	 =   5.5383; // +/- 5.22288e-06 cm/us All: West = 0.192827 +/- 0.00105197
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5383 +/- 5.22288e-06
  return (TDataSet *)tableSet;// West = 5.53844 +/- 5.91477e-06 East = 5.53779 +/- 1.11284e-05
};
