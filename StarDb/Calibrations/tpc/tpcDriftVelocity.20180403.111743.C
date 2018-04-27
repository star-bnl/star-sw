TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 93014
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52005; // +/- 1.49787e-05 cm/us All: East = -0.586469 +/- 0.00787456
  row.laserDriftVelocityWest	 =   5.52005; // +/- 1.49787e-05 cm/us All: West = 0.290191 +/- 0.00297365
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52005 +/- 1.49787e-05
  return (TDataSet *)tableSet;// West = 5.51945 +/- 1.60133e-05 East = 5.52425 +/- 4.23595e-05
};
