TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 168044
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54629; // +/- 6.41936e-06 cm/us All: East = -0.423498 +/- 0.00421891
  row.laserDriftVelocityWest	 =   5.54629; // +/- 6.41936e-06 cm/us All: West = 0.29412 +/- 0.00120197
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54629 +/- 6.41936e-06
  return (TDataSet *)tableSet;// West = 5.54599 +/- 6.67591e-06 East = 5.54996 +/- 2.33806e-05
};
