TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 157018
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5531; // +/- 4.80404e-06 cm/us All: East = 0.267096 +/- 0.00196891
  row.laserDriftVelocityWest	 =   5.5531; // +/- 4.80404e-06 cm/us All: West = 0.16814 +/- 0.000935386
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5531 +/- 4.80404e-06
  return (TDataSet *)tableSet;// West = 5.5532 +/- 5.32907e-06 East = 5.55265 +/- 1.10992e-05
};
