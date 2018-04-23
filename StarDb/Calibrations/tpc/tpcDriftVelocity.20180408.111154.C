TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 98018
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54808; // +/- 1.35404e-05 cm/us All: East = -0.775232 +/- 0.0108105
  row.laserDriftVelocityWest	 =   5.54808; // +/- 1.35404e-05 cm/us All: West = 0.252132 +/- 0.0024998
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54808 +/- 1.35404e-05
  return (TDataSet *)tableSet;// West = 5.54775 +/- 1.39454e-05 East = 5.55346 +/- 5.65925e-05
};
