TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 88066
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52701; // +/- 1.29726e-05 cm/us All: East = -0.682359 +/- 0.00935687
  row.laserDriftVelocityWest	 =   5.52701; // +/- 1.29726e-05 cm/us All: West = 0.261087 +/- 0.0024293
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52701 +/- 1.29726e-05
  return (TDataSet *)tableSet;// West = 5.52664 +/- 1.34607e-05 East = 5.53187 +/- 4.86131e-05
};
