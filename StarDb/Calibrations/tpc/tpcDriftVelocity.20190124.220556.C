TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 24037
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55688; // +/- 1.0532e-05 cm/us All: East = 0.984774 +/- 0.00378192
  row.laserDriftVelocityWest	 =   5.55688; // +/- 1.0532e-05 cm/us All: West = 1.17057 +/- 0.0021397
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55688 +/- 1.0532e-05
  return (TDataSet *)tableSet;// West = 5.55658 +/- 1.1885e-05 East = 5.55796 +/- 2.27288e-05
};
