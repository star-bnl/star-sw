TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 98060
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54059; // +/- 3.59768e-05 cm/us All: East = 0.0100702 +/- 0.0154347
  row.laserDriftVelocityWest	 =   5.54059; // +/- 3.59768e-05 cm/us All: West = 0.355957 +/- 0.0150685
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54059 +/- 3.59768e-05
  return (TDataSet *)tableSet;// West = 5.53962 +/- 5.33457e-05 East = 5.54139 +/- 4.87253e-05
};
