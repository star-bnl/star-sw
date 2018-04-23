TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 105043
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55406; // +/- 5.11992e-05 cm/us All: East = 0.000374131 +/- 0.0437978
  row.laserDriftVelocityWest	 =   5.55406; // +/- 5.11992e-05 cm/us All: West = 0.278752 +/- 0.0315199
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55406 +/- 5.11992e-05
  return (TDataSet *)tableSet;// West = 5.55341 +/- 7.13132e-05 East = 5.55476 +/- 7.35519e-05
};
