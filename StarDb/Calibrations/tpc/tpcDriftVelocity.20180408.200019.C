TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 98037
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53963; // +/- 2.20549e-05 cm/us All: East = -0.0936373 +/- 0.0144704
  row.laserDriftVelocityWest	 =   5.53963; // +/- 2.20549e-05 cm/us All: West = 0.233914 +/- 0.0043649
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53963 +/- 2.20549e-05
  return (TDataSet *)tableSet;// West = 5.53929 +/- 2.44192e-05 East = 5.5411 +/- 5.13775e-05
};
