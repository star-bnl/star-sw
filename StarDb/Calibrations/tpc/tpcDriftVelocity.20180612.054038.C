TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 163004
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54736; // +/- 1.00753e-05 cm/us All: East = -0.566602 +/- 0.00572339
  row.laserDriftVelocityWest	 =   5.54736; // +/- 1.00753e-05 cm/us All: West = 0.258177 +/- 0.00193269
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54736 +/- 1.00753e-05
  return (TDataSet *)tableSet;// West = 5.5468 +/- 1.07567e-05 East = 5.55138 +/- 2.87655e-05
};
