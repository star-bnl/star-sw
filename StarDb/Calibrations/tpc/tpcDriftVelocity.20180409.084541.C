TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 99010
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.551; // +/- 1.61991e-05 cm/us All: East = -5.76182 +/- 0.00607348
  row.laserDriftVelocityWest	 =   5.551; // +/- 1.61991e-05 cm/us All: West = -4.92728 +/- 0.00330461
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.551 +/- 1.61991e-05
  return (TDataSet *)tableSet;// West = 5.54997 +/- 1.8328e-05 East = 5.55465 +/- 3.46294e-05
};
