TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 107033
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5556; // +/- 1.29939e-05 cm/us All: East = 0.0829814 +/- 0.00282811
  row.laserDriftVelocityWest	 =   5.5556; // +/- 1.29939e-05 cm/us All: West = 0.382159 +/- 0.00429611
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5556 +/- 1.29939e-05
  return (TDataSet *)tableSet;// West = 5.55442 +/- 2.40862e-05 East = 5.55609 +/- 1.54321e-05
};
