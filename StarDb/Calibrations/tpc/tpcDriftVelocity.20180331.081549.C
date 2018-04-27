TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 90010
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.51721; // +/- 1.01219e-05 cm/us All: East = -1.09807 +/- 0.376123
  row.laserDriftVelocityWest	 =   5.51721; // +/- 1.01219e-05 cm/us All: West = 0.168911 +/- 0.00181084
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.51721 +/- 1.01219e-05
  return (TDataSet *)tableSet;// West = 5.51718 +/- 1.01487e-05 East = 5.52338 +/- 0.000139339
};
