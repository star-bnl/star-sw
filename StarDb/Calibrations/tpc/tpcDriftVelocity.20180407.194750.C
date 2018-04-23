TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 97051
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54657; // +/- 1.31877e-05 cm/us All: East = 0.00390655 +/- 0.00347191
  row.laserDriftVelocityWest	 =   5.54657; // +/- 1.31877e-05 cm/us All: West = 0.298607 +/- 0.00327142
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54657 +/- 1.31877e-05
  return (TDataSet *)tableSet;// West = 5.54583 +/- 1.80411e-05 East = 5.54741 +/- 1.93256e-05
};
