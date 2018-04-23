TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 99047
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55285; // +/- 1.95697e-05 cm/us All: East = -0.0626166 +/- 0.0040047
  row.laserDriftVelocityWest	 =   5.55285; // +/- 1.95697e-05 cm/us All: West = 0.659349 +/- 0.00798396
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55285 +/- 1.95697e-05
  return (TDataSet *)tableSet;// West = 5.54973 +/- 3.99318e-05 East = 5.55383 +/- 2.24506e-05
};
