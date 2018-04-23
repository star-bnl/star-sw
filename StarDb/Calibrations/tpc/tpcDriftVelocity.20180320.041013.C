TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 79001
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.56489; // +/- 6.88342e-06 cm/us All: East = 0.138067 +/- 0.0057264
  row.laserDriftVelocityWest	 =   5.56489; // +/- 6.88342e-06 cm/us All: West = 0.163007 +/- 0.00126477
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.56489 +/- 6.88342e-06
  return (TDataSet *)tableSet;// West = 5.5649 +/- 7.12375e-06 East = 5.56481 +/- 2.67264e-05
};
