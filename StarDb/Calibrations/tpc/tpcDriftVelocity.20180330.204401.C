TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 89051
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52549; // +/- 1.05998e-05 cm/us All: East = -0.579424 +/- 0.00960136
  row.laserDriftVelocityWest	 =   5.52549; // +/- 1.05998e-05 cm/us All: West = 0.225273 +/- 0.00194467
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52549 +/- 1.05998e-05
  return (TDataSet *)tableSet;// West = 5.5253 +/- 1.08335e-05 East = 5.52974 +/- 5.13083e-05
};
