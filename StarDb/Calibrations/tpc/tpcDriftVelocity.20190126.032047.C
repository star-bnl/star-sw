TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 25038
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55446; // +/- 2.28031e-05 cm/us All: East = 0.598539 +/- 0.0151842
  row.laserDriftVelocityWest	 =   5.55446; // +/- 2.28031e-05 cm/us All: West = 1.28363 +/- 0.00685769
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55446 +/- 2.28031e-05
  return (TDataSet *)tableSet;// West = 5.55257 +/- 3.67847e-05 East = 5.55565 +/- 2.90605e-05
};
