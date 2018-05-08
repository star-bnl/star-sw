TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 124063
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54121; // +/- 6.66687e-06 cm/us All: East = 0.237374 +/- 0.00214871
  row.laserDriftVelocityWest	 =   5.54121; // +/- 6.66687e-06 cm/us All: West = 0.23942 +/- 0.00141702
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54121 +/- 6.66687e-06
  return (TDataSet *)tableSet;// West = 5.54121 +/- 7.95171e-06 East = 5.5412 +/- 1.22322e-05
};
