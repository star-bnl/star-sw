TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 81040
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53534; // +/- 1.47083e-05 cm/us All: East = 0.783881 +/- 0.0112336
  row.laserDriftVelocityWest	 =   5.53534; // +/- 1.47083e-05 cm/us All: West = 1.22594 +/- 0.0027227
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53534 +/- 1.47083e-05
  return (TDataSet *)tableSet;// West = 5.53519 +/- 1.51485e-05 East = 5.53774 +/- 6.14565e-05
};
