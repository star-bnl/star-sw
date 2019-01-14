TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 12039
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5373; // +/- 1.47611e-05 cm/us All: East = 1.40809 +/- 0.00539585
  row.laserDriftVelocityWest	 =   5.5373; // +/- 1.47611e-05 cm/us All: West = 1.87884 +/- 0.00308641
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5373 +/- 1.47611e-05
  return (TDataSet *)tableSet;// West = 5.53671 +/- 1.68195e-05 East = 5.53929 +/- 3.07932e-05
};
