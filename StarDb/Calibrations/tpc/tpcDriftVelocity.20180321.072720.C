TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 80008
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.56146; // +/- 1.11304e-05 cm/us All: East = -2.47751 +/- 0.0173692
  row.laserDriftVelocityWest	 =   5.56146; // +/- 1.11304e-05 cm/us All: West = -3.50038 +/- 0.00196666
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.56146 +/- 1.11304e-05
  return (TDataSet *)tableSet;// West = 5.56146 +/- 1.11304e-05 East = 5.559 +/- 0.0114896
};
