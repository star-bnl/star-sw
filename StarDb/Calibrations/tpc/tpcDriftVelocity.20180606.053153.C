TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 157003
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55327; // +/- 5.18885e-06 cm/us All: East = 0.291851 +/- 0.00182134
  row.laserDriftVelocityWest	 =   5.55327; // +/- 5.18885e-06 cm/us All: West = 0.140704 +/- 0.00106205
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55327 +/- 5.18885e-06
  return (TDataSet *)tableSet;// West = 5.55348 +/- 6.02936e-06 East = 5.55267 +/- 1.01884e-05
};
