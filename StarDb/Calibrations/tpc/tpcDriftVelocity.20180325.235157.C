TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 84063
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52119; // +/- 1.10415e-05 cm/us All: East = -0.306428 +/- 0.00793992
  row.laserDriftVelocityWest	 =   5.52119; // +/- 1.10415e-05 cm/us All: West = 0.199576 +/- 0.00205144
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52119 +/- 1.10415e-05
  return (TDataSet *)tableSet;// West = 5.52103 +/- 1.13853e-05 East = 5.5238 +/- 4.52759e-05
};
