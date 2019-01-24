TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 23019
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5644; // +/- 8.74168e-06 cm/us All: East = -0.22886 +/- 0.00309766
  row.laserDriftVelocityWest	 =   5.5644; // +/- 8.74168e-06 cm/us All: West = -0.0832302 +/- 0.00179244
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5644 +/- 8.74168e-06
  return (TDataSet *)tableSet;// West = 5.5642 +/- 1.0049e-05 East = 5.565 +/- 1.77235e-05
};
