TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 106037
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54564; // +/- 1.16421e-05 cm/us All: East = -0.182483 +/- 0.00302388
  row.laserDriftVelocityWest	 =   5.54564; // +/- 1.16421e-05 cm/us All: West = 0.123487 +/- 0.00293579
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54564 +/- 1.16421e-05
  return (TDataSet *)tableSet;// West = 5.54483 +/- 1.6244e-05 East = 5.5465 +/- 1.66941e-05
};
