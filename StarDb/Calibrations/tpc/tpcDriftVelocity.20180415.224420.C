TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 105043
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54411; // +/- 4.91876e-05 cm/us All: East = -0.155517 +/- 0.0342475
  row.laserDriftVelocityWest	 =   5.54411; // +/- 4.91876e-05 cm/us All: West = 0.156309 +/- 0.0295528
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54411 +/- 4.91876e-05
  return (TDataSet *)tableSet;// West = 5.54349 +/- 6.4869e-05 East = 5.54495 +/- 7.54468e-05
};
