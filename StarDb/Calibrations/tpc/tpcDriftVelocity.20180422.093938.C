TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 112016
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5532; // +/- 7.46416e-06 cm/us All: East = -0.101966 +/- 0.00389323
  row.laserDriftVelocityWest	 =   5.5532; // +/- 7.46416e-06 cm/us All: West = 0.380647 +/- 0.00142107
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5532 +/- 7.46416e-06
  return (TDataSet *)tableSet;// West = 5.55294 +/- 7.91076e-06 East = 5.55533 +/- 2.25338e-05
};
