TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 108045
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54432; // +/- 1.05964e-05 cm/us All: East = -0.632452 +/- 0.00469013
  row.laserDriftVelocityWest	 =   5.54432; // +/- 1.05964e-05 cm/us All: West = 0.209563 +/- 0.00140924
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54432 +/- 1.05964e-05
  return (TDataSet *)tableSet;// West = 5.54348 +/- 1.16952e-05 East = 5.54814 +/- 2.50401e-05
};
