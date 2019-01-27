TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 27014
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53433; // +/- 8.86721e-06 cm/us All: East = 4.18597 +/- 0.00516778
  row.laserDriftVelocityWest	 =   5.53433; // +/- 8.86721e-06 cm/us All: West = 4.59964 +/- 0.00166523
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53433 +/- 8.86721e-06
  return (TDataSet *)tableSet;// West = 5.53416 +/- 9.36078e-06 East = 5.53586 +/- 2.76729e-05
};
