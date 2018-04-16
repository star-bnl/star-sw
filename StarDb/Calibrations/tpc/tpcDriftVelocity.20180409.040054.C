TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 99001
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55092; // +/- 1.32508e-05 cm/us All: East = -5.25056 +/- 0.00308737
  row.laserDriftVelocityWest	 =   5.55092; // +/- 1.32508e-05 cm/us All: West = -4.88574 +/- 0.00382465
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55092 +/- 1.32508e-05
  return (TDataSet *)tableSet;// West = 5.54975 +/- 2.0695e-05 East = 5.55174 +/- 1.72508e-05
};
