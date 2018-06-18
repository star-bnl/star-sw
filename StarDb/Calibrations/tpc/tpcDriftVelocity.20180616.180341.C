TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 167031
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54676; // +/- 5.48785e-06 cm/us All: East = 0.228006 +/- 0.00266473
  row.laserDriftVelocityWest	 =   5.54676; // +/- 5.48785e-06 cm/us All: West = 0.195245 +/- 0.00104185
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54676 +/- 5.48785e-06
  return (TDataSet *)tableSet;// West = 5.54677 +/- 5.88803e-06 East = 5.54665 +/- 1.51443e-05
};
