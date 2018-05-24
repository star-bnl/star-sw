TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 139027
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55507; // +/- 5.29837e-06 cm/us All: East = 0.225995 +/- 0.00527963
  row.laserDriftVelocityWest	 =   5.55507; // +/- 5.29837e-06 cm/us All: West = 0.0659991 +/- 0.000943848
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55507 +/- 5.29837e-06
  return (TDataSet *)tableSet;// West = 5.5551 +/- 5.39188e-06 East = 5.55419 +/- 2.85727e-05
};
