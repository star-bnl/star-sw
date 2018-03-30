TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 74016
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52075; // +/- 8.83679e-06 cm/us All: East = 3.46104 +/- 0.00976639
  row.laserDriftVelocityWest	 =   5.52075; // +/- 8.83679e-06 cm/us All: West = 3.85502 +/- 0.00160066
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52075 +/- 8.83679e-06
  return (TDataSet *)tableSet;// West = 5.52069 +/- 8.95297e-06 East = 5.52292 +/- 5.503e-05
};
