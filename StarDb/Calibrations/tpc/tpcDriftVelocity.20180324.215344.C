TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 83055
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53411; // +/- 9.86221e-06 cm/us All: East = -0.245846 +/- 0.00790878
  row.laserDriftVelocityWest	 =   5.53411; // +/- 9.86221e-06 cm/us All: West = 0.209301 +/- 0.00182091
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53411 +/- 9.86221e-06
  return (TDataSet *)tableSet;// West = 5.53397 +/- 1.01382e-05 East = 5.5365 +/- 4.25593e-05
};
