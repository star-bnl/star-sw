TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 162037
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54739; // +/- 8.30923e-06 cm/us All: East = -0.368255 +/- 0.00419855
  row.laserDriftVelocityWest	 =   5.54739; // +/- 8.30923e-06 cm/us All: West = 0.23967 +/- 0.0015809
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54739 +/- 8.30923e-06
  return (TDataSet *)tableSet;// West = 5.54697 +/- 8.90266e-06 East = 5.5502 +/- 2.31463e-05
};
