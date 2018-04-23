TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 106016
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55477; // +/- 1.58041e-05 cm/us All: East = -1.39561 +/- 16.2723
  row.laserDriftVelocityWest	 =   5.55477; // +/- 1.58041e-05 cm/us All: West = 0.182511 +/- 0.00283401
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55477 +/- 1.58041e-05
  return (TDataSet *)tableSet;// West = 5.55477 +/- 1.58041e-05 East = 5.55834 +/- 0.00749144
};
