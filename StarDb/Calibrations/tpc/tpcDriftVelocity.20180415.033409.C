TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 104047
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54286; // +/- 9.40355e-06 cm/us All: East = -0.32102 +/- 0.00421952
  row.laserDriftVelocityWest	 =   5.54286; // +/- 9.40355e-06 cm/us All: West = 0.299439 +/- 0.00184759
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54286 +/- 9.40355e-06
  return (TDataSet *)tableSet;// West = 5.5423 +/- 1.02942e-05 East = 5.54568 +/- 2.31117e-05
};
