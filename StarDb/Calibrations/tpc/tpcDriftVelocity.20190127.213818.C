TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 27063
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53102; // +/- 7.57954e-06 cm/us All: East = 0.574743 +/- 0.00269741
  row.laserDriftVelocityWest	 =   5.53102; // +/- 7.57954e-06 cm/us All: West = 0.841088 +/- 0.0015604
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53102 +/- 7.57954e-06
  return (TDataSet *)tableSet;// West = 5.53069 +/- 8.7752e-06 East = 5.53199 +/- 1.50409e-05
};
