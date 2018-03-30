TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 79001
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.56479; // +/- 6.86011e-06 cm/us All: East = -4.03286 +/- 0.00511056
  row.laserDriftVelocityWest	 =   5.56479; // +/- 6.86011e-06 cm/us All: West = -4.10106 +/- 0.0012581
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.56479 +/- 6.86011e-06
  return (TDataSet *)tableSet;// West = 5.5648 +/- 7.07708e-06 East = 5.56453 +/- 2.79194e-05
};
